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

{-| This module introduces the 'Script' and 'Trace' types.  A 'Script'
is a list of HTTP requests that can be printed for inspection,
serialized, or compiled to python.  It can also be run through an
interpreter that produces a 'Trace'.  The story contains the responses
from the server together with the requests.

The most common way of using this is to write an arbitrary instance
for a newtype-wrapper around 'Script', and a set of properties on
stories.

The requests in a 'Script' depend on each other, and there is a
reference mechanism for accessing objects that are returned from the
server earlier for constructing later requests.  Both legal and
illegal (as far as the REST counterpart is concerned) request
sequences can be tested (the properties are free to expect either
success or error in any context).

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
import Test.QuickCheck.Store
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

-- | One request in a 'Script'.  For convenience, the body may be
-- stored in a typed way, and will be serialized using 'ToJSON', but
-- it is also possible to write requests with arbitrarily broken
-- bodies.  The serial number is used for extracting request paths
-- from earlier responses, and for property checking in stories (see
-- 'Script' type for more info).  The path is either a 'URI', or a
-- reference that is passed to a path constructor function (see
-- below).
--
-- For instance, you may reference an earlier @POST@ request, retrieve
-- the path of an object that has been stored, and use that path in a
-- @GET@ request to retrieve it.
--
-- (Future work: constructor 'DynScriptItem' makes use of past
-- responses (not only requests) in order to generate requests
-- dynamically (won't work in python compiler).
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
        , siHTTPPath     :: Either URI PathRef
        }
{-
    | ScriptItemDWD
        { siSerial       :: Ix
        , siDWD          :: (MonadIO wd, WebDriver wd) => wd (Either Element content)
        }
-}
  deriving (Show, Eq, Typeable, Generic)

instance (Cereal.Serialize sid, Cereal.Serialize content)
    => Cereal.Serialize (ScriptItem sid content)

-- | Serial numbers for 'Script's and 'Trace's.
newtype Ix = Ix { fromIx :: Int }
  deriving (Eq, Ord, Enum, Typeable, Generic)

instance Show Ix where
    showsPrec n (Ix ix) = showsPrec n ix

instance Read Ix where
    readsPrec n s = case readsPrec n s of [(i, s)] -> [(Ix i, s)]

-- | PathRef either refers to an 'Ix', or to a dedicated root 'URI' (see
-- below).
data PathRef = PathRef Ix | PathRefRoot
  deriving (Show, Eq, Ord, Typeable, Generic)

instance Cereal.Serialize Ix
instance Cereal.Serialize PathRef

pathRef :: Either URI PathRef -> Maybe Ix
pathRef (Right (PathRef x)) = Just x
pathRef _ = Nothing



-- * Scripts

-- | A 'Script' is a list of requests.  Scripts are run head-to-last.
-- The 'ScriptItem' type has a serial number.  Serial numbers are not
-- continuous (possibly N is not used, but N+1 is), not monotonous
-- (numbers may alternatingly increase and decrease over time), and
-- unique (no serial number is used twice).
newtype Script sid content = Script { scriptItems :: [ScriptItem sid content] }
  deriving (Show, Eq, Typeable, Generic)

instance (Cereal.Serialize sid, Cereal.Serialize content)
    => Cereal.Serialize (Script sid content)

-- | (In script concatenation, no sanitation of serial numbers of
-- pathrefs is taking place.  FIXME: at least check if it's sound?
-- anyway, we need another abstraction layer here.  think about this
-- some more!)
instance Monoid (Script sid content) where
  mappend (Script xs) (Script ys) = Script $ xs ++ ys
  mempty = Script []

getScriptItem :: Script sid content -> Ix -> Maybe (ScriptItem sid content)
getScriptItem (Script rqs) serial = case filter ((== serial) . siSerial) rqs of
    []   -> Nothing
    [rq] -> Just rq


-- ** Constructing scripts

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

-- | Generate arbitrary 'Script's.  This is more complicated than
-- 'listOf', since the path refs are context sensitive.  Takes a list
-- of serial numbers of script items in earlier segments.
arbitraryScript :: forall sid content . (Arbitrary content, Show content)
                => RefCtx -> Maybe [(Int, RequestMethod)] -> Gen (Script sid content)
arbitraryScript context desiredMethods = Script . reverse . snd <$>
      (listOf (arbitrary :: Gen ()) >>= foldM f (context, []))
  where
    f :: (RefCtx, [ScriptItem sid content]) -> a -> Gen (RefCtx, [ScriptItem sid content])
    f (ctx, rqs) _ = do
        rq <- arbitraryScriptItem ctx desiredMethods
        return (ctx <> scriptItemContext rq, rq:rqs)

-- | In a given 'RefCtx' and with an (optional) given request method
-- distribution, construct a request with incrementing serial numbers,
-- arbitrary but sound references, arbitrary but sound methods and
-- contents, and empty get and post params and headers.
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
      pathref    <- fmap Right . QC.elements . Set.toList $ refCtx ctx
      method   <- QC.frequency . map (second pure) . maybe defaultMethods id $ desiredMethods
      content  <- Right <$> arbitrary

      return $ ScriptItemHTTP ix Nothing Nothing method content [] [] [] pathref


instance (Arbitrary content, Show content, Eq sid, Eq content) => Arbitrary (Script sid content) where
  arbitrary = arbitraryScript mempty Nothing
  shrink (Script xs) = nub $ dropDanglingReferences . Script <$> shrink xs

instance (Arbitrary content, Show content, Eq content) => Arbitrary (ScriptItem sid content) where
  arbitrary = arbitraryScriptItem mempty Nothing
  shrink rq = case siBody rq of
                Left s   -> upd <$> [Left ""]
                Right c  -> upd . Right <$> shrink c
    where
      upd :: Either SBS content -> ScriptItem sid content
      upd b = rq { siBody = b }


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

-- | leaves serial number and path ref alone.  breaking the former is
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
-- numbers in scope, (2) all valid paths (PathRefRoot and the serial
-- numbers of all requests that have created (not modified) a path by
-- method @POST@ or @PUT@), and (3) all deleted paths (method
-- @DELETE@).
data RefCtx = RefCtx
      { ctxSerials :: Set Ix
      , ctxAddedPaths :: Set PathRef
      , ctxRemovedPaths :: Set PathRef
      }
  deriving (Show, Eq, Ord, Generic)

-- | The empty context contains 'PathRefRoot' in the set of added refs.
-- Append crashes if the two sets of serial numbers overlap.  (FIXME:
-- is this comment visible in haddock?  where?)
instance Monoid RefCtx where
    mempty = RefCtx Set.empty (Set.singleton PathRefRoot) Set.empty
    mappend :: RefCtx -> RefCtx -> RefCtx
    mappend (RefCtx serials added removed) (RefCtx serials' added' removed')
           | (Set.null $ Set.intersection serials serials') =
               RefCtx (Set.union serials serials') (Set.union added added') (Set.union removed removed')

-- | The set of all valid references in a given context.
refCtx :: RefCtx -> Set PathRef
refCtx ctx = ctxAddedPaths ctx Set.\\ ctxRemovedPaths ctx

scriptContext :: (Show content) => Script sid content -> RefCtx
scriptContext = foldl (<>) mempty . map scriptItemContext . scriptItems

-- | If item is a POST or PUT, add pathref to "added"; if it is a
-- DELETE, add to "removed"; otherwise, neither.  (FIXME: This may or
-- may not be what you need.  This library should offer something more
-- configurable.)
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



-- * Stories

-- | A 'Trace' is an interpreted 'Script'.  Trace items (or events, or
-- actions) consist of a 'ScriptItemHTTP' and an HTTP response.  The are
-- arranged in a list in the same order as in the 'Script', i.e. last
-- item was played last.  See 'runScript'.
newtype Trace sid content = Trace { traceItems  :: [TraceItem sid content] }
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
      f :: TraceItem sid content -> String
      f (TraceItemHTTP rq rsp) = ">>>>>\n" <> ppShow rq <> "\n"
                 <> case rsp of
                      Just rsp_ -> "<<<<<\n" <> show rsp_ <> "\n" <> cs (rspBody rsp_) <> "\n"
                      Nothing -> "<<<<<\n(skipped)\n"

instance Monoid (Trace sid content) where
  mappend (Trace xs) (Trace ys) = Trace $ xs ++ ys
  mempty = Trace []


getTraceItem :: Trace sid content -> Ix -> Either (TraceError sid content) (TraceItem sid content)
getTraceItem (Trace trace) serial =
    case filter ((== serial) . siSerial . tiScriptItem) trace of
        [result@(TraceItemHTTP rq (Just rsp@(rspCode -> (2, _, _))))] -> Right result
        [result@(TraceItemHTTP rq (Just rsp@(rspCode -> code)))]      -> Left (TraceErrorHTTP code)
        [result@(TraceItemHTTP rq Nothing)]                           -> Left (TraceErrorSkipped rq)
        []                                                            -> Left TraceErrorSerialNotFound


data TraceError sid content =
      TraceErrorSerialNotFound
    | TraceErrorHTTP ResponseCode
    | TraceErrorSkipped (ScriptItem sid content)
    | TraceErrorJSON
  deriving (Show, Eq)



-- * Reference reduction

-- | Headers and body of a 'ScriptItemHTTP' may contain (sub-)strings of the
-- form @___SCRIPT_REF___<i>@, or @___SCRIPT_REF___@, which index into
-- earlier 'Script' items (the latter into 'PathRefRoot').  This
-- function finds those, calls a transformer on each occurrance, and
-- substitutes it by the resulting string.  It accepts a 'RefCtx' so
-- that references to non-existing items can be answered with a
-- comprehensive error message.  The first argument states whether
-- surrounding quotes (double or single) should be matched away
-- together with the reference string.
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


-- | Render 'PathRef' into a string suitable for being found by
-- 'reduceRefs'.  The boolean flag is for double-quotation (yes
-- means double-quotes).
scriptRefToSBS :: Bool -> PathRef -> LBS
scriptRefToSBS False PathRefRoot = "___SCRIPT_REF___"
scriptRefToSBS False (PathRef i) = "___SCRIPT_REF___" <> cs (show i)
scriptRefToSBS True  PathRefRoot = "\"___SCRIPT_REF___\""
scriptRefToSBS True  (PathRef i) = "\"___SCRIPT_REF___" <> cs (show i) <> "\""


-- | Dynamic 'reduceRefs', as needed for 'Trace'.  (See 'reduceRefsPy'
-- for a static variant.)
reduceRefsDyn :: forall sid content . (Show content, JS.FromJSON content, JS.ToJSON content)
         => Bool -> URI -> (TraceItem sid content -> Maybe URI) -> Trace sid content
         -> SBS -> SBS
reduceRefsDyn quoted rootPath constructPath story = cs . reduceRefs quoted ctx (fmap (qu . ru) . lu) . cs
  where
    script :: Script sid content  = Script . map tiScriptItem $ traceItems story
    ctx    :: RefCtx              = scriptContext script

    lu :: PathRef -> Maybe URI   -- lookup xref
    lu PathRefRoot = Just rootPath
    lu (PathRef i) = case filter ((== i) . siSerial . tiScriptItem) $ traceItems story of
                     [item] -> constructPath item

    ru :: URI -> LBS     -- render uri
    ru = cs . uriPath

    qu :: LBS -> LBS     -- re-quote (if applicable)
    qu = if quoted then cs . show else id


reduceRefsDynAssoc :: forall sid content . (Show content, JS.FromJSON content, JS.ToJSON content)
         => URI -> (TraceItem sid content -> Maybe URI) -> Trace sid content
         -> [(SBS, SBS)] -> [(SBS, SBS)]
reduceRefsDynAssoc root construct story = map (first f . second f)
  where f = reduceRefsDyn False root construct story


reduceRefsDynBody :: forall sid content . (Show content, JS.FromJSON content, JS.ToJSON content)
         => URI -> (TraceItem sid content -> Maybe URI) -> Trace sid content
         -> Either SBS content -> Either SBS content
reduceRefsDynBody root construct story (Left s) =
    Left (reduceRefsDyn True root construct story s)
reduceRefsDynBody root construct story (Right c) =
    sanitizeScriptItemContent . Left . reduceRefsDyn True root construct story . cs $ JS.encode c



-- * interpreter

-- | Run a 'Script' and return a list of pairs of 'ScriptItemHTTP' and HTTP
-- responses with serial.  Requests on paths missing due to earlier
-- errors are skipped (response is 'Nothing').  See 'Script' for
-- properties of serial numbers.
--
-- (possible optimization: make runScript take a property of stories
-- that is evaluated after every response on the already-constructed
-- story prefix.  write a function that generates properties from such
-- a property of traces and a machine to run it on.  these properties
-- would be a more direct approach to constructing counter-examples
-- than implementing properties on stories and calling runScript and
-- quickCheck manually.  FIXME: reconsider this idea once the
-- implemention of the ideas by Hughes/Claessen are implemented and
-- understood.)
runScript :: forall sid content . (Show sid, Show content, JS.FromJSON content, JS.ToJSON content)
          => Bool -> URI -> (TraceItem sid content -> Maybe URI) -> Script sid content -> IO (Trace sid content)
runScript verbose rootPath constructPath (Script rs) = foldM f (Trace []) rs
  where
    f :: Trace sid content -> ScriptItem sid content -> IO (Trace sid content)
    f trace rq@(ScriptItemHTTP serial _ _ method body getparams postparams headers pathref) = do
            let pathMay :: Maybe URI
                        = case pathref of
                            Right (PathRef ix) ->
                              case getTraceItem trace ix of
                                 Right item                     -> constructPath item
                                 Left TraceErrorSerialNotFound  -> error $ "runScript: dangling pathref: " ++ ppShow (pathref, trace)
                                 Left (TraceErrorHTTP _)        -> Nothing
                                 Left (TraceErrorSkipped _)     -> Nothing
                            Right PathRefRoot ->
                              Just rootPath
                            Left uri ->
                              Just uri

            case pathMay of
              Just path -> do
                let body'       = reduceRefsDynBody rootPath constructPath trace body
                    getparams'  = evl getparams
                    postparams' = evl postparams
                    headers'    = evl headers
                    evl         = reduceRefsDynAssoc rootPath constructPath trace

                response <- performReq verbose method path getparams' postparams' headers' body'
                return $ trace <> Trace [TraceItemHTTP rq (Just response)]
              Nothing -> do
                return $ trace <> Trace [TraceItemHTTP rq Nothing]



-- * Compile to python

scriptToPyFile :: (JS.ToJSON content, Show content) => Script sid content -> FilePath -> IO ()
scriptToPyFile script filepath = SBS.writeFile filepath . SBS.intercalate "\n" . scriptToPySBS $ script

scriptToPySBS :: (JS.ToJSON content, Show content) => Script sid content -> [SBS]
scriptToPySBS (Script rqs) = pyHeader ++ rqs' ++ ["", "print 'success!'", ""]
  where
    rqs' :: [SBS]
    rqs' = concatMap (uncurry scriptItemToPy) $ zip ctxs rqs

    ctxs :: [RefCtx]
    ctxs = map (scriptContext . Script) $ inits rqs

pyHeader :: [SBS]
pyHeader =
    "#!/usr/bin/python" :
    "# -*- encoding: utf-8 -*-" :
    "" :
    "import os" :
    "import json" :
    "import requests" :
    "" :
    "null = None  # for more javascript-ish json representation" :
    "" :
    []

-- | FIXME: get params and put params are not supported.
scriptItemToPy :: (JS.ToJSON content) => RefCtx -> ScriptItem sid content -> [SBS]
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

    showRef :: Either URI PathRef -> SBS
    showRef (Right (PathRef i))  = "resp_" <> cs (show i) <> ".json()['path']"
    showRef (Right PathRefRoot)  = "rootpath"
    showRef (Left uri)         = cs $ show uri

    path :: SBS = "server + " <> showRef r

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
        => (a -> Script sid content) -> IO ()
compileStore toScript = readStore toScript >>= mapM_ (compileTestCase toScript)


-- | Test cases that are newtype-wrapped 'Script's can be compiled to
-- both Haskell data (for inspection and modification) and python (for
-- convenience and inclusion in external test workflows).
compileTestCase :: forall a sid content . (Show a, Typeable a, Cereal.Serialize a,
                                           Show sid, Show content, JS.ToJSON content)
        => (a -> Script sid content) -> (FilePath, Script sid content) -> IO ()
compileTestCase _ (filename, testData) = do
    writeFile (dropExtension filename <.> "hs") $ ppShow testData
    scriptToPyFile testData (dropExtension filename <.> "py")



-- * More helpers

getScriptItemHTTPContent :: ScriptItem sid content -> Maybe content
getScriptItemHTTPContent (ScriptItemHTTP _ _ _ _ (Right body) _ _ _ _) = Just body
getScriptItemHTTPContent _ = Nothing


-- | ratio of 200 responses vs. all others.
errorRate :: Trace sid content -> Double
errorRate (Trace trace) = fromIntegral (length errors) / fromIntegral (length trace)
  where
    errors = filter (\ (TraceItemHTTP _ rsp) -> (rspCode <$> rsp) /= Just (2, 0, 0)) trace


-- | ratio of requests not sent due to earlier errors vs. all others.
skipRate :: Trace sid content -> Double
skipRate (Trace trace) = fromIntegral (length skipped) / fromIntegral (length trace)
  where
    skipped = filter (isNothing . tiEffectHTTP) trace
