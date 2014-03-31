{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving               #-}
{-# LANGUAGE InstanceSigs                             #-}
{-# LANGUAGE NoImplicitPrelude                        #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE ViewPatterns                             #-}

{-# OPTIONS -fwarn-unused-imports -fno-warn-incomplete-patterns #-}

{-| This module introduces the types 'Script' and 'Trace'.  A 'Script'
is a list of events called 'ScriptItem's (HTTP requests, browser
events, etc.).  A 'Script' can be printed for inspection, serialized,
compiled to python, or (most interestingly) run through an interpreter
that produces a 'Trace'.  The 'Trace' contains the events together
with their effects (responses from the backend, browser, etc.).

Both legal and illegal (as far as the REST counterpart is concerned)
request sequences can be tested (the properties are free to expect
either success or error in any context).

The requests in a 'Script' depend on each other, and there is a
reference mechanism (see e.g. reduceRefsTrace), for using earlier
effects in the constructions of later events.

[1] Koen Claessen, John Hughes, Testing Monadic Code with
    QuickCheck, Department of Computer Science, Chalmers University of
    Technology

-}
module Test.WebApp.Script
where

import Control.Applicative
import Control.Arrow
import Control.Monad hiding (mapM)
import Data.Char
import Data.Data
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import Data.String.Conversions
import Data.Traversable (mapM)
import GHC.Generics
import Network.HTTP
import Network.URI
import Prelude hiding (mapM)
import Safe
import System.Directory
import System.FilePath
import Test.QuickCheck as QC
import Test.QuickCheck.Store as QC
import Text.Printf
import Text.Regex.Easy
import Text.Show.Pretty

import qualified Data.Aeson as JS
import qualified Data.Aeson.Encode.Pretty as JS
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Serialize as Cereal
import qualified Data.Set as Set hiding (Set)

import Test.QuickCheck.Missing
import Test.WebApp.Arbitrary
import Test.WebApp.HTTP.Util
import Test.WebApp.Orphans ()



-- * Script requests

-- | One event in a 'Script'.  The serial number is needed when
-- constructing later script items from earlier ones and when writing
-- 'Trace' properties.  See 'Script' type for more info on serial
-- numbers.
--
-- HTTP: For convenience, the body may be stored in a typed way, and
-- will be serialized using 'ToJSON', but it is also possible to write
-- requests with arbitrarily broken bodies.  The path is either a path
-- string, or a reference that is passed to a path constructor
-- function (see below).
--
-- For instance, you may reference an earlier @POST@ request, retrieve
-- the path of an object that has been stored, and use that path in a
-- @GET@ request to retrieve it.
--
-- WebDriver: coming up...
--
-- (Future work: constructor 'ScriptItemRT' (RunTime) makes use of
-- past effects (not only script items) in order to generate script items
-- dynamically.  This constructor will lack many nice features such as
-- compilability to python.)
data ScriptItem sid content =
      ScriptItemHTTP
        { siSerial       :: Ix
        , siFromState    :: Maybe sid
        , siThisState    :: Maybe sid
        , siMethod       :: RequestMethod
        , siBody         :: Either SBS content
        , siGetParams    :: [(SBS, SBS)]
        , siPostParams   :: [(SBS, SBS)]
        , siHeaders      :: [(SBS, SBS)]
        , siHTTPPath     :: Either Path PathRef
        }
{-
    | ScriptItemDWD
        { siSerial       :: Ix
        , siDWD          :: (MonadIO wd, WebDriver wd) => wd (Either Element content)
        }
-}
  deriving (Show, Eq, Typeable, Generic)

type ScriptItem' = ScriptItem Int JS.Value

instance (Cereal.Serialize sid, Cereal.Serialize content)
    => Cereal.Serialize (ScriptItem sid content)

-- | Serial numbers for 'Script's and 'Trace's.
newtype Ix = Ix { fromIx :: Int }
  deriving (Eq, Ord, Enum, Typeable, Generic)

instance Show Ix where
    showsPrec n (Ix ix) = showsPrec n ix

instance Read Ix where
    readsPrec n s = case readsPrec n s of [(i, s)] -> [(Ix i, s)]

newtype Path = Path { fromPath :: SBS }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Cereal.Serialize Path

instance Arbitrary Path where
    arbitrary = Path <$> arbitrary
    shrink (Path p) = Path <$> shrink p

instance Fuzz Path where
    fuzz (Path p) = Path <$> fuzz p

instance JS.FromJSON Path where
    parseJSON (JS.String s) = return . Path $ cs s

instance JS.ToJSON Path where
    toJSON (Path p) = JS.String $ cs p

-- | PathRef either refers to an 'Ix', or to a dedicated root URI (see
-- below).
data PathRef = PathRef Ix | PathRefRoot
  deriving (Show, Eq, Ord, Typeable, Generic)

instance Cereal.Serialize Ix
instance Cereal.Serialize PathRef

pathRef :: Either Path PathRef -> Maybe Ix
pathRef (Right (PathRef x)) = Just x
pathRef _ = Nothing



-- * Scripts

-- | A 'Script' is a list of requests.  Scripts are run head-to-last.
-- The 'ScriptItem' type has a serial number.  Serial numbers are
-- unique (no serial number is used twice), but not continuous
-- (possibly N is not used, but N+1 is) and not monotonous (numbers
-- may alternatingly increase and decrease over time).
newtype Script sid content = Script { scriptItems :: [ScriptItem sid content] }
  deriving (Show, Eq, Typeable, Generic)

type Script' = Script Int JS.Value

instance (Cereal.Serialize sid, Cereal.Serialize content)
    => Cereal.Serialize (Script sid content)

-- | In script concatenation, no sanitation of serial numbers or
-- pathrefs is taking place.  (FIXME: this should either crash on
-- unsound concatenations or sanitize them where possible.)
instance Monoid (Script sid content) where
  mappend (Script xs) (Script ys) = Script $ xs ++ ys
  mempty = Script []


-- ** Constructing scripts

-- | Very basic DSL for writing scripts in a more convenient form
-- (this is work in progress).
newScript :: forall sid content . [(String, String, Ix -> PathRef -> ScriptItem sid content)] -> Script sid content
newScript = Script . f (Ix 0) [("/", PathRefRoot)]
  where
    f :: Ix
      -> [(String, PathRef)]
      -> [(String, String, Ix -> PathRef -> ScriptItem sid content)]
      -> [ScriptItem sid content]
    f _ _ [] = []
    f i ctx ((label, refstring, x):xs) =
        case lookup refstring ctx of
            Just pathref -> x i pathref : f (nextIx i) ((label, PathRef i):ctx) xs
            Nothing -> error $ printf "item %i: could not find label %s" (fromIx i) (show label)

nextIx :: Ix -> Ix
nextIx = Ix . (+1) . fromIx

nextIxCtx :: RefCtx -> Ix
nextIxCtx (RefCtx serials _ _) = Ix . maybe 0 ((+1) . fst) . Set.maxView . Set.map fromIx $ serials

mapSerials :: forall sid content . (Ix -> Ix) -> (Script sid content) -> (Script sid content)
mapSerials f (Script rqs) = Script $ map q rqs
  where
    q :: ScriptItem sid content -> ScriptItem sid content
    q rq = rq { siHTTPPath = case siHTTPPath rq of
                            x@(Left e) -> x
                            x@(Right PathRefRoot) -> x
                            (Right (PathRef r)) -> Right (PathRef $ f r) }

-- | Parse body from lines of json code.
readReqBody :: JS.FromJSON content => [LBS] -> Either SBS content
readReqBody (LBS.intercalate "\n" -> s) = maybe (Left $ cs s) (Right) $ JS.decode s

emptyReqBody :: Either SBS content
emptyReqBody = Left ""

newLabelledItem ::
     String
  -> RequestMethod -> String -> Either SBS content -> [(SBS, SBS)] -> [(SBS, SBS)] -> [(SBS, SBS)]
  -> (String, String, Ix -> PathRef -> ScriptItem sid content)
newLabelledItem label meth refstring body getparams postparams headers =
    (label, refstring, \ ix ref -> ScriptItemHTTP ix Nothing Nothing meth body getparams postparams headers (Right ref))

newItem ::
     RequestMethod -> String -> Either SBS content -> [(SBS, SBS)] -> [(SBS, SBS)] -> [(SBS, SBS)]
  -> (String, String, Ix -> PathRef -> ScriptItem sid content)
newItem = newLabelledItem ""

sanitizeScript :: JS.FromJSON content => Script sid content -> Script sid content
sanitizeScript (Script rqs) = Script $ map sanitizeScriptItem rqs

sanitizeScriptItem :: JS.FromJSON content => ScriptItem sid content -> ScriptItem sid content
sanitizeScriptItem r@(ScriptItemHTTP _ _ _ _ b _ _ _ _) = r { siBody = sanitizeScriptItemContent b }

sanitizeScriptItemContent :: JS.FromJSON content => Either SBS content -> Either SBS content
sanitizeScriptItemContent (Right c) = Right c
sanitizeScriptItemContent (Left s) = maybe (Left s) (Right) . JS.decode $ cs s


-- ** Arbitrary scripts

instance (Arbitrary content, Show content, Eq sid, Eq content) => Arbitrary (Script sid content) where
  arbitrary = arbitraryScript mempty Nothing
  shrink = shrinkScript

instance (Arbitrary content, Show content, Eq content) => Arbitrary (ScriptItem sid content) where
  arbitrary = arbitraryScriptItem mempty Nothing
  shrink = shrinkScriptItem


-- | Generate arbitrary 'Script's.  This is slightly more
-- sophisticated than 'listOf', since the path refs are context
-- sensitive.  Takes a 'RefCtx' and an (optional) probability
-- distribution over request methods.
arbitraryScript :: forall sid content . (Arbitrary content, Show content)
          => RefCtx -> Maybe [(Int, RequestMethod)] -> Gen (Script sid content)
arbitraryScript ctx desiredMethods = Script . reverse . fst <$>
      (listOf (arbitrary :: Gen ()) >>= foldM f ([], ctx))
  where
    f :: ([ScriptItem sid content], RefCtx) -> () -> Gen ([ScriptItem sid content], RefCtx)
    f (rqs, ctx) () = do
        rq <- arbitraryScriptItem ctx desiredMethods
        return (rq:rqs, ctx <> scriptItemContext rq)


-- | The default shrink method for 'Script's is to shrink the list of
-- script items and call 'dropDanglingReferences' on all outcomes.
shrinkScript :: (Eq sid, Eq content, Show content, Arbitrary content)
          => Script sid content -> [Script sid content]
shrinkScript = nub . fmap (dropDanglingReferences . Script) . shrink . scriptItems


-- | In a given 'RefCtx' and with an (optional) given request method
-- distribution, construct a request with fresh serial number,
-- arbitrary but sound 'PathRef', arbitrary but sound method and
-- content, and empty get and post params and headers.  If request
-- method distribution is 'Nothing', a plausible default is used.
arbitraryScriptItem :: forall sid content . (Arbitrary content, Show content)
          => RefCtx -> Maybe [(Int, RequestMethod)] -> Gen (ScriptItem sid content)
arbitraryScriptItem ctx desiredMethods = do
      let defaultMethods :: [(Int, RequestMethod)]
          defaultMethods = [ (7, GET)
                           , (5, PUT)
                           , (5, POST)
                           , (2, DELETE)
                           , (1, OPTIONS)
                           , (1, HEAD)
                           ]

      let ix    = nextIxCtx ctx
      pathref  <- fmap Right . QC.elements . Set.toList $ refCtx ctx
      method   <- QC.frequency . map (second pure) . maybe defaultMethods id $ desiredMethods
      content  <- Right <$> arbitrary

      return $ ScriptItemHTTP ix Nothing Nothing method content [] [] [] pathref


-- | Default shrink method for 'ScriptItem's just shrinks the content.
shrinkScriptItem :: forall sid content . (Arbitrary content)
          => ScriptItem sid content -> [ScriptItem sid content]
shrinkScriptItem item =
        case siBody item of
          Left s   -> upd <$> [Left ""]
          Right c  -> upd . Right <$> shrink c
    where
      upd :: Either SBS content -> ScriptItem sid content
      upd b = item { siBody = b }


-- | All scripts must have distinct serial numbers.
prop_arbitraryScriptIx :: (Script sid content) -> Property
prop_arbitraryScriptIx (Script rqs) = mkprop $ length rqs == (length . nub . map siSerial $ rqs)


-- | All scripts must not have dangling references.
prop_arbitraryScriptPathRef :: (Script sid content) -> Property
prop_arbitraryScriptPathRef (Script rqs) = mkprop $ refs `Set.isSubsetOf` ixs
  where
    refs = Set.fromList . catMaybes . map (pathRef . siHTTPPath) $ rqs
    ixs = Set.fromList . map siSerial $ rqs


-- ** Fuzzing scripts

instance (Fuzz content, JS.ToJSON content) => Fuzz (Script sid content) where
  fuzz = fuzzLastNRqs 10

fuzzLastNRqs :: (Fuzz content, JS.ToJSON content) => Int -> Script sid content -> Gen (Script sid content)
fuzzLastNRqs n (Script rqs) = do
      i <- choose (0, max 0 (length rqs - n))
      case splitAt i rqs of
        (notToBeFuzzed, toBeFuzzed) -> Script . (notToBeFuzzed ++) <$> mapM fuzz toBeFuzzed


-- | Leave serial number and path ref alone.  breaking the former is
-- not interesting, because it would only attack consistency of the
-- test code, not the code under test.  breaking the latter is more
-- interesting if we have a 'RefCtx' to do it, so we need to do
-- it when fuzzing a 'Script'.
--
-- FIXME: fuzz path refs, get and post params, ...
instance forall sid content . (Fuzz content, JS.ToJSON content) => Fuzz (ScriptItem sid content) where
  fuzz (ScriptItemHTTP i _ _ m b gps pps h r) = oneof [tweakMethod, tweakBody, tweakHeaders]
    where
      tweakMethod :: Gen (ScriptItem sid content)
      tweakMethod = (\ m' -> ScriptItemHTTP i Nothing Nothing m' b gps pps h r)
               <$> arbitrary

      tweakBody :: Gen (ScriptItem sid content)
      tweakBody = (\ b' -> ScriptItemHTTP i Nothing Nothing m (Left b') gps pps h r)
               <$> either fuzz (fuzz . (cs :: LBS -> SBS) . JS.encode) b

      tweakHeaders :: Gen (ScriptItem sid content)
      tweakHeaders = (\ h' -> ScriptItemHTTP i Nothing Nothing m b gps pps h' r) <$> forM h (\ (k, v) ->
            frequency [ (50, (k,) <$> fuzz v)
                      , (50, (,v) <$> fuzz k)
                      ])


-- ** Script contexts

-- | The context of a script at any given point in the request list
-- with respect to references is defined by three sets: (1) All serial
-- numbers in scope, (2) all added path refs (PathRefRoot and the
-- serial numbers of all requests that have created (not modified) a
-- path using methods @POST@, @PUT@), and (3) all removed path refs
-- (method @DELETE@).  The set of active path refs is defined as
-- @added \\ removed@ (see 'refCtx').
data RefCtx = RefCtx
      { ctxSerials :: Set Ix
      , ctxAddedPaths :: Set PathRef
      , ctxRemovedPaths :: Set PathRef
      }
  deriving (Show, Eq, Ord, Generic)

-- | The empty context contains 'PathRefRoot' in the set of added
-- refs.  Append crashes if the two sets of serial numbers overlap.
instance Monoid RefCtx where
    mempty = RefCtx Set.empty (Set.singleton PathRefRoot) Set.empty
    mappend :: RefCtx -> RefCtx -> RefCtx
    mappend (RefCtx serials added removed) (RefCtx serials' added' removed')
           | (Set.null $ Set.intersection serials serials') =
               RefCtx (Set.union serials serials') (Set.union added added') (Set.union removed removed')

-- | The set of all active references (@added \\ removed@) in a given
-- context.
refCtx :: RefCtx -> Set PathRef
refCtx ctx = ctxAddedPaths ctx Set.\\ ctxRemovedPaths ctx

-- | Derive the context from a 'Script'.
scriptContext :: (Show content) => Script sid content -> RefCtx
scriptContext = foldl (<>) mempty . map scriptItemContext . scriptItems

-- | Derive the context from a 'ScriptItem'.  If item is a POST or
-- PUT, add pathref to "added"; if it is a DELETE, add to "removed";
-- otherwise, neither.  (FIXME: This may or may not be what you need.
-- This library should offer something more configurable.)
scriptItemContext :: (Show content) => ScriptItem sid content -> RefCtx
scriptItemContext rq = RefCtx (Set.singleton (siSerial rq)) added removed
    where
        yes :: Set PathRef
        yes = Set.singleton . PathRef . siSerial $ rq

        no :: Set PathRef
        no = Set.empty

        added :: Set PathRef
        removed :: Set PathRef
        (added, removed) = case (siHTTPPath rq, siMethod rq) of
                    (Right _, POST)   -> (yes, no)
                    (Right _, PUT)    -> (yes, no)
                    (Right _, DELETE) -> (no, yes)
                    (_, _)            -> (no,  no)


-- ** Reference maintenance

dropDanglingReferences :: (Show content, Eq content) => Script sid content -> Script sid content
dropDanglingReferences = dropDanglingReferences'3


newtype DropDanglingReferences = DropDanglingReferences (Script Int Int)
  deriving (Show, Eq, Typeable, Generic)

instance Cereal.Serialize DropDanglingReferences

instance Arbitrary DropDanglingReferences where
  arbitrary = DropDanglingReferences <$> arbitrary
  shrink (DropDanglingReferences x) = DropDanglingReferences <$> shrink x


-- | A nano-study of different ways of iterating through a list with
-- state.
prop_DropDanglingReferences :: DropDanglingReferences -> QC.Property
prop_DropDanglingReferences (DropDanglingReferences script) = mkprop . all good . shrink_ $ script
  where
    shrink_ = join . take 7 . shrink . join . take 7 . shrink . take 7 . shrink
    good :: (Show content, Eq sid, Eq content) => Script sid content -> Bool
    good script = dropDanglingReferences'1 script == dropDanglingReferences'2 script &&
                  dropDanglingReferences'2 script == dropDanglingReferences'3 script


-- | Variant 1: Peek into prefix of generated list.
dropDanglingReferences'1 :: forall sid content . Script sid content -> Script sid content
dropDanglingReferences'1 (Script rs) = Script $ catMaybes rs'
  where
    -- Set elements whose path ref points to a deleted item to
    -- 'Nothing'.  This way, we can 'take' the first @i@
    -- elements of rs' during construction and always be sure
    -- not to run into a loop.
    rs' :: [Maybe (ScriptItem sid content)]
    rs' = map (\ (i, r) ->
                case siHTTPPath r of
                  Left _           -> Just r
                  Right PathRefRoot  -> Just r
                  Right (PathRef ix) -> let context = map siSerial . catMaybes . take i $ rs' in
                                      if ix `elem` context
                                          then Just r
                                          else Nothing)
              $ zip [0..] rs


-- | Variant 2: 'foldl'.  ('foldr' could make do without the
-- 'reverse'; not sure about space complexity.)
dropDanglingReferences'2 :: forall sid content . (Show content) => Script sid content -> Script sid content
dropDanglingReferences'2 = Script . reverse . snd . foldl f (mempty, []) . scriptItems
  where
    f :: (RefCtx, [ScriptItem sid content]) -> ScriptItem sid content -> (RefCtx, [ScriptItem sid content])
    f (ctx, rqs) rq = if maybe False (`Set.member` ctxAddedPaths ctx) . fmap PathRef . pathRef . siHTTPPath $ rq
                          then (ctx <> scriptItemContext rq, rq:rqs)
                          else (ctx, rqs)


-- | Variant 3: tail recursion.  This is my favorite :)
dropDanglingReferences'3 :: forall sid content . (Show content) => Script sid content -> Script sid content
dropDanglingReferences'3 (Script rqs) = Script $ f mempty rqs
  where
    f :: RefCtx -> [ScriptItem sid content] -> [ScriptItem sid content]
    f ctx [] = []
    f ctx (rq:rqs) = if maybe False (`Set.member` ctxAddedPaths ctx) . fmap PathRef . pathRef . siHTTPPath $ rq
                            then rq  : f (ctx <> scriptItemContext rq) rqs
                            else f ctx rqs



-- * Traces

-- | A 'Trace' is an interpreted 'Script'.  'TraceItem's are arranged
-- in a list in the same order as in the 'Script', i.e. last item was
-- played last, and associated with an optional test outcome.  See
-- 'runScript'', 'runScript'.
newtype Trace sid content = Trace { traceItems  :: [(TraceItem sid content, Maybe Bool)] }
  deriving (Typeable)

data TraceItem sid content =
      TraceItemHTTP
        { tiScriptItem :: ScriptItem sid content
        , tiEffectHTTP :: Maybe (Response LBS)
        }
  deriving (Show, Typeable, Generic)

instance (Show sid, Show content) => Show (Trace sid content) where
  show (Trace [])       = "(empty Trace)\n"
  show (Trace xs@(_:_)) = ("Trace\n  " <>) . intercalate "\n  " . lines . concat . map f $ xs
    where
      f :: (TraceItem sid content, Maybe Bool) -> String
      f (TraceItemHTTP rq rsp, check) = ">>>>>\n" <> ppShow rq <> "\n"
                 <> case rsp of
                      Just rsp_ -> "<<<<<\n" <> show rsp_ <> "\n" <> (cs . rspBody . prettyRespBody $ rsp_) <> "\n"
                      Nothing -> "<<<<<\n(skipped)\n"
                 <> "<<<<<   [" <> show check <> "]\n"

instance Monoid (Trace sid content) where
  mappend (Trace xs) (Trace ys) = Trace $ xs ++ ys
  mempty = Trace []


getTraceItem :: Trace sid content -> Ix -> Either (TraceError sid content) (TraceItem sid content)
getTraceItem (Trace trace) serial =
    case filter ((== serial) . siSerial . tiScriptItem . fst) trace of
        [fst -> result@(TraceItemHTTP rq (Just rsp@(rspCode -> (2, _, _))))] -> Right result
        [fst -> result@(TraceItemHTTP rq (Just rsp@(rspCode -> code)))]      -> Left (TraceErrorHTTP code)
        [fst -> result@(TraceItemHTTP rq Nothing)]                           -> Left (TraceErrorSkipped rq)
        []                                                                   -> Left TraceErrorSerialNotFound


data TraceError sid content =
      TraceErrorSerialNotFound
    | TraceErrorHTTP ResponseCode
    | TraceErrorSkipped (ScriptItem sid content)
    | TraceErrorJSON
  deriving (Show, Eq)



-- * Reference reduction

-- | Headers and body of a 'ScriptItemHTTP' may contain (sub-)strings
-- of the form @___SCRIPT_REF___<i>@, or @___SCRIPT_REF___@, which
-- index into earlier 'Script' items and into 'PathRefRoot',
-- respectively.  'reduceRefs' calls a transformer on each occurrance
-- and substitutes it by the resulting string.  It accepts a 'RefCtx'
-- so that references to non-existing items can trigger an informative
-- error message.
--
-- The first argument states whether surrounding quotes (double or
-- single) should be matched away together with the reference string
-- (this is necessary for code generation where a string literal is to
-- be replaced by a string variable).
reduceRefs :: Bool -> RefCtx -> (PathRef -> Maybe LBS) -> LBS -> LBS
reduceRefs quoted ctx rewrite js =
      if null badrefs
        then replaceRegexAll js pattern (\ match -> extract quoted match >>= rewrite)
        else error $ "reduceRefs: illegal references: " ++ show badrefs
  where
    pattern :: SBS = q <> "___SCRIPT_REF___(\\d+)?" <> q
      where q = if quoted then "(\"|\')" else ""

    extract :: Bool -> [(LBS, (MatchOffset, MatchLength))] -> Maybe PathRef
    extract False [_, (readMay . cs . fst) -> Just i]                         = Just $ PathRef i
    extract False [_, ("", (-1, 0))]                                          = Just $ PathRefRoot
    extract True  [_, ("'", _),  (readMay . cs . fst) -> Just i, ("'", _)]    = Just $ PathRef i
    extract True  [_, ("'", _),  ("", (-1, 0)),                  ("'", _)]    = Just $ PathRefRoot
    extract True  [_, ("\"", _), (readMay . cs . fst) -> Just i, ("\"", _)]   = Just $ PathRef i
    extract True  [_, ("\"", _), ("", (-1, 0)),                  ("\"", _)]   = Just $ PathRefRoot
    extract _     _                                                           = Nothing

    refs :: Set Ix
         = Set.fromList
         . catMaybes . map (pathRef . Right)
         . catMaybes . map (extract quoted)
         $ js =~++ pattern

    badrefs :: [Ix]
         = Set.toList $ refs Set.\\ ctxSerials ctx


-- | (Just a quick in-place unit test for 'reduceRefs'.)
test_reduceRefs :: Bool
test_reduceRefs = f True == ["PathRefRoot","PathRef 0"] && f False == ["PathRefRoot","PathRef 0"]
  where
    f quote = map (reduceRefs quote ctx (Just . cs . show) . scriptRefToSBS quote) refs
    ctx = RefCtx (Set.fromList [Ix 0]) (Set.fromList refs) Set.empty
    refs = [PathRefRoot, PathRef (Ix 0)]


-- | Render 'PathRef' into a string suitable for being replaced by
-- 'reduceRefs'.  This is needed for things like header strings
-- containing path refs.  The boolean flag is for double-quotation
-- (yes means double-quotes).
scriptRefToSBS :: Bool -> PathRef -> LBS
scriptRefToSBS False PathRefRoot = "___SCRIPT_REF___"
scriptRefToSBS False (PathRef i) = "___SCRIPT_REF___" <> cs (show i)
scriptRefToSBS True  PathRefRoot = "\"___SCRIPT_REF___\""
scriptRefToSBS True  (PathRef i) = "\"___SCRIPT_REF___" <> cs (show i) <> "\""


-- | Dynamic 'reduceRefs', as needed for 'runScript''.  (See 'reduceRefsPy'
-- for a static variant.)
reduceRefsTrace :: forall sid content . (Show content, JS.FromJSON content, JS.ToJSON content)
         => Bool -> Path -> (TraceItem sid content -> Maybe Path) -> Trace sid content
         -> SBS -> SBS
reduceRefsTrace quoted rootPath constructPath trace = cs . reduceRefs quoted ctx (fmap qu . lu) . cs
  where
    script :: Script sid content  = Script . map (tiScriptItem . fst) $ traceItems trace
    ctx    :: RefCtx              = scriptContext script

    lu :: PathRef -> Maybe Path   -- lookup xref
    lu PathRefRoot = Just rootPath
    lu (PathRef i) = case filter ((== i) . siSerial . tiScriptItem . fst) $ traceItems trace of
                     [fst -> item] -> constructPath item

    qu :: Path -> LBS     -- re-quote (if applicable)
    qu = cs . (if quoted then show else cs) . fromPath


reduceRefsTraceAssoc :: forall sid content . (Show content, JS.FromJSON content, JS.ToJSON content)
         => Path -> (TraceItem sid content -> Maybe Path) -> Trace sid content
         -> [(SBS, SBS)] -> [(SBS, SBS)]
reduceRefsTraceAssoc root construct trace = map (first f . second f)
  where f = reduceRefsTrace False root construct trace


reduceRefsTraceBody :: forall sid content . (Show content, JS.FromJSON content, JS.ToJSON content)
         => Path -> (TraceItem sid content -> Maybe Path) -> Trace sid content
         -> Either SBS content -> Either SBS content
reduceRefsTraceBody root construct trace (Left s) =
    Left (reduceRefsTrace True root construct trace s)
reduceRefsTraceBody root construct trace (Right c) =
    sanitizeScriptItemContent . Left . reduceRefsTrace True root construct trace . cs $ JS.encode c



-- * interpreter

data RunScriptSetup sid content =
    RunScriptSetup
      { runVerbose      :: Bool
      , runRootURI      :: URI
      , runExtractPath  :: TraceItem sid content -> Maybe Path
      }


runScriptMkURI :: RunScriptSetup sid content -> Path -> URI
runScriptMkURI setup (Path path) = root'
  where
    root = runRootURI setup
    path = uriPath root
    path' = path <> cs path
    root' = root { uriPath = path'}


-- | Run a 'Script' and return a 'Trace'.  For every 'TraceItem', a
-- boolean test outcome is computed from it and the 'Trace' history,
-- and associated with the 'TraceItem' in the new history.  Requests
-- on paths missing due to earlier errors are skipped (effect is
-- 'Nothing').
runScript' :: forall sid content . (Show sid, Show content, JS.FromJSON content, JS.ToJSON content)
          => RunScriptSetup sid content
          -> Script sid content
          -> (TraceItem sid content -> Trace sid content -> Maybe Bool)
          -> IO (Trace sid content)
runScript' setup@(RunScriptSetup verbose rootURI extractPath) (Script items) test = foldM f (Trace []) items
  where
    f :: Trace sid content -> ScriptItem sid content -> IO (Trace sid content)
    f trace rq@(ScriptItemHTTP _ _ _ method body getparams postparams headers pathref) = do
            let pathMay :: Maybe URI
                        = case pathref of
                            Right (PathRef ix) ->
                              case getTraceItem trace ix of
                                 Right item                     -> runScriptMkURI setup <$> extractPath item
                                 Left TraceErrorSerialNotFound  -> error $ "runScript': dangling pathref: " ++ ppShow (pathref, trace)
                                 Left (TraceErrorHTTP _)        -> Nothing
                                 Left (TraceErrorSkipped _)     -> Nothing
                            Right PathRefRoot ->
                              Just rootURI
                            Left path ->
                              Just $ runScriptMkURI setup path

            case pathMay of
              Just path -> do
                let rootPath    = Path . cs . uriPath $ rootURI
                    body'       = reduceRefsTraceBody rootPath extractPath trace body
                    getparams'  = evl getparams
                    postparams' = evl postparams
                    headers'    = evl headers
                    evl         = reduceRefsTraceAssoc rootPath extractPath trace

                response <- performReq verbose method path getparams' postparams' headers' body'

                let traceItem = TraceItemHTTP rq (Just response)
                    checkResult = test traceItem trace

                return $ trace <> Trace [(traceItem, checkResult)]
              Nothing -> do
                return $ trace <> Trace [(TraceItemHTTP rq Nothing, Nothing)]


-- | Run a 'Script' and return a 'Trace'.  Requests on paths missing
-- due to earlier errors are skipped (effect is 'Nothing').  Check
-- results are 'Nothing' for all 'TraceItem's.
runScript :: forall sid content . (Show sid, Show content, JS.FromJSON content, JS.ToJSON content)
          => RunScriptSetup sid content
          -> Script sid content
          -> IO (Trace sid content)
runScript (RunScriptSetup verbose rootPath extractPath) (Script items) =
    runScript' (RunScriptSetup verbose rootPath extractPath) (Script items) (\ _ _ -> Nothing)


-- | Transform a property of 'Trace's (which you usually would want to
-- write) into a property of 'Script's (which are more
-- straight-forward to run).  This also requires a setup object naming
-- verbosity level, server coordinates, ...
dynamicScriptProp :: forall sid content . (Show sid, Show content, JS.FromJSON content, JS.ToJSON content)
          => (Trace sid content -> Property) -> RunScriptSetup sid content -> (Script sid content -> Property)
dynamicScriptProp prop setup script =
    QC.ioProperty $ prop <$> runScript setup script


-- FIXME: dynamicScriptProp'?  (it acts to runScript' as
-- dynamicScriptProp acts to runScript.  (hm...  what does that even
-- mean?))



-- * Compile to python

scriptToPyFile :: (Show sid, Show content, JS.ToJSON content)
               => RunScriptSetup sid content -> Script sid content -> FilePath -> IO ()
scriptToPyFile setup script filepath = SBS.writeFile filepath . SBS.intercalate "\n" $ scriptToPySBS setup script

scriptToPySBS :: (Show sid, Show content, JS.ToJSON content)
              => RunScriptSetup sid content -> Script sid content -> [SBS]
scriptToPySBS setup (Script rqs) = pyHeader setup ++ rqs' ++ ["", "print 'success!'", ""]
  where
    rqs' :: [SBS]
    rqs' = concatMap (uncurry scriptItemToPy) $ zip ctxs rqs

    ctxs :: [RefCtx]
    ctxs = map (scriptContext . Script) $ inits rqs

pyHeader :: RunScriptSetup sid content -> [SBS]
pyHeader setup =
    "#!/usr/bin/python" :
    "# -*- encoding: utf-8 -*-" :
    "" :
    "import os" :
    "import json" :
    "import requests" :
    "" :
    "uri_rootpath = " <> (cs . show . uriPath . runRootURI $ setup) :
    "uri_stem = " <> (cs . show . show $ (runRootURI setup) { uriPath = "" }) :
    "null = None  # for more javascript-ish json representation" :
    "" :
    []

-- | FIXME: get params and put params are not supported.
scriptItemToPy :: (Show sid, Show content, JS.ToJSON content)
               => RefCtx -> ScriptItem sid content -> [SBS]
scriptItemToPy _ scriptItem@(ScriptItemHTTP (Ix x) _ _ _ _ _ _ _ _) | x < 0 =
    error $ "scriptItemToPy: invalid serial number in item: " ++ show scriptItem
scriptItemToPy ctx (ScriptItemHTTP x _ _ m b [] [] hs r) =
    ("data = " <> case either cs (cs . reduceRefsPy True ctx . JS.encodePretty) b of
                    "" -> "''"
                    s -> "json.dumps(" <> s <> ")") :
    "print '====================================================================== [REQUEST]'" :
    "print '  method: ' + " <> mss :
    "print '  uri: ' + " <> path :
    "print '  data: ' + data" :
    (resp <> " = requests." <> ms <> "(" <> path <> ", data=data, headers=" <> headers <> ")") :
    "print ''" :
    "" :
    "print '---------------------------------------------------------------------- [RESPONSE]'" :
    "print '  code: ' + str(" <> resp <> ".status_code)" :
    "if " <> resp <> ".status_code == 200:" :
    "    print '  data: ' + json.dumps(" <> resp <> ".json())" :
    "    print ''" :
    "" :
    "else:" :
    "    print '  data: ' + " <> resp <> ".text" :
    "    print ''" :
    "    print 'giving up!'" :
    "    exit(1)" :
    "" :
    []
  where
    ms  :: SBS = cs . map toLower . show $ m
    mss :: SBS = (<> "'") . ("'" <>) $ ms
    xs  :: SBS = cs . show $ x
    rs  :: SBS = cs . show $ r

    resp :: SBS = "resp_" <> xs
    path :: SBS = "uri_stem + " <> showRef r
      where
        showRef :: Either Path PathRef -> SBS
        showRef (Right (PathRef i))  = "resp_" <> cs (show i) <> ".json()['path']"
        showRef (Right PathRefRoot)  = "rootpath"
        showRef (Left (Path path))   = cs $ show path

    headers :: SBS = cs . mconcat $
                  "{" :
                  "'content-type': 'text/json'," :
                  map render hs ++
                  "}" :
                  []
      where
        render :: (SBS, SBS) -> SBS
        render (k, v) | not ('\'' `elem` cs (k <> v) || '\\' `elem` cs (k <> v))
            = "    '" <> k <> "': " <> (cs . reduceRefsPy False ctx . cs $ v) <> ","

    reduceRefsPy :: Bool -> RefCtx -> LBS -> LBS
    reduceRefsPy quoted ctx = reduceRefs quoted ctx f
      where
        f PathRefRoot = Just "rootpath"
        f (PathRef i) = Just $ "resp_" <> cs (show i) <> ".json()['path']"



-- * Quickcheck Store helpers

-- | List all unit test cases in the DB, together with their filenames.
readStore :: forall a sid content . (Show a, Typeable a, Cereal.Serialize a)
        => (a -> Script sid content) -> IO [(FilePath, Script sid content)]
readStore toScript = do
    let path = (storeRoot </> show (typeOf (undefined :: a)))
    filenames <- filter (not . (`elem` [".", ".."])) <$> getDirectoryContents path
    mapM (\ filename -> (path </> filename,) . toScript <$> decodeConfidently (path </> filename)) filenames


-- | Given a function that transforms a given type into a 'Script',
-- compile all test cases of that type into Haskell and Python.
compileStore :: forall a sid content . (Show a, Typeable a, Cereal.Serialize a,
                                        Show sid, Show content, JS.ToJSON content)
        => RunScriptSetup sid content -> (a -> Script sid content) -> IO ()
compileStore setup toScript = readStore toScript >>= mapM_ (compileTestCase setup)


-- | Test cases that are newtype-wrapped 'Script's can be compiled to
-- both Haskell data (for inspection and modification) and python (for
-- convenience and inclusion in external test workflows).
compileTestCase :: forall sid content . (Show sid, Show content, JS.ToJSON content)
        => RunScriptSetup sid content -> (FilePath, Script sid content) -> IO ()
compileTestCase setup (filename, testData) = do
    writeFile (dropExtension filename <.> "hs") $ ppShow testData
    scriptToPyFile setup testData (dropExtension filename <.> "py")



-- * More helpers

getScriptItem :: Script sid content -> Ix -> Maybe (ScriptItem sid content)
getScriptItem (Script rqs) serial = case filter ((== serial) . siSerial) rqs of
    []   -> Nothing
    [rq] -> Just rq


getScriptItemContent :: ScriptItem sid content -> Maybe content
getScriptItemContent (ScriptItemHTTP _ _ _ _ (Right body) _ _ _ _) = Just body
getScriptItemContent (ScriptItemHTTP _ _ _ _ _ _ _ _ _)            = Nothing


getScriptContent :: Script sid content -> Ix -> Maybe content
getScriptContent (Script rqs) serial = case filter ((== serial) . siSerial) rqs of
    []   -> Nothing
    [rq] -> getScriptItemContent rq


-- | ratio of 200 responses vs. all others.
errorRate :: Trace sid content -> Double
errorRate (Trace trace) = fromIntegral (length errors) / fromIntegral (length trace)
  where
    errors = filter (\ (TraceItemHTTP _ rsp, _) -> (rspCode <$> rsp) /= Just (2, 0, 0)) trace


-- | ratio of requests not sent due to earlier errors vs. all others.
skipRate :: Trace sid content -> Double
skipRate (Trace trace) = fromIntegral (length skipped) / fromIntegral (length trace)
  where
    skipped = filter (isNothing . tiEffectHTTP . fst) trace
