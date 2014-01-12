{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE InstanceSigs                             #-}
{-# LANGUAGE NoImplicitPrelude                        #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE ViewPatterns                             #-}

{-# OPTIONS -fwarn-unused-imports -fno-warn-incomplete-patterns #-}

{-| This module introduces the 'Script' and 'Story' types.  A 'Script'
is a list of HTTP requests that can be printed for inspection,
serialized, or compiled to python.  It can also be run through an
interpreter that produces a 'Story'.  The story contains the responses
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
module Test.WebApp.HTTP.Story
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
import Test.WebApp.HTTP.Util
import Test.WebApp.Orphans ()



-- * script requests

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
-- (Future work: constructor 'DynScriptRq' makes use of past
-- responses (not only requests) in order to generate requests
-- dynamically (won't work in python compiler).
data ScriptRq c =
      ScriptRq
        { srqSerial       :: Ix
        , srqMethod       :: RequestMethod
        , srqBody         :: Either SBS c
        , srqGetParams    :: [(SBS, SBS)]
        , srqPostParams   :: [(SBS, SBS)]
        , srqHeaders      :: [(SBS, SBS)]
        , srqPath         :: Either URI IxRef
        }
  deriving (Show, Eq, Typeable, Generic)

instance (Cereal.Serialize c) => Cereal.Serialize (ScriptRq c)

-- | Serial numbers for 'Script's and 'Story's.
type Ix = Int

-- | IxRef either refers to an 'Ix', or to a dedicated root 'URI' (see
-- below).
data IxRef = IxRef Int | IxRefRoot
  deriving (Show, Eq, Ord, Typeable, Generic)

instance Cereal.Serialize IxRef

fromIxRef :: IxRef -> Maybe Int
fromIxRef (IxRef x) = Just x
fromIxRef IxRefRoot = Nothing

srqPathRef :: Either URI IxRef -> Maybe IxRef
srqPathRef (Right r) = Just r
srqPathRef (Left _) = Nothing

srqPathSerial :: Either URI IxRef -> Maybe Ix
srqPathSerial (Right (IxRef x)) = Just x
srqPathSerial _ = Nothing



-- * scripts

-- | A 'Script' is a list of requests.  Scripts are run head-to-last.
-- The 'ScriptRq' type has a serial number.  Serial numbers are not
-- continuous (possibly N is not used, but N+1 is), not monotonous
-- (numbers may alternatingly increase and decrease over time), and
-- unique (no serial number is used twice).
newtype Script c = Script { fromScript :: [ScriptRq c] }
  deriving (Show, Eq, Typeable, Generic)

instance (Cereal.Serialize c) => Cereal.Serialize (Script c)

-- | (In script concatenation, no sanitation of serial numbers of
-- pathrefs is taking place.  FIXME: at least check if it's sound?
-- anyway, we need another abstraction layer here.  think about this
-- some more!)
instance Monoid (Script c) where
  mappend (Script xs) (Script ys) = Script $ xs ++ ys
  mempty = Script []

getScriptRq :: Script c -> Ix -> Maybe (ScriptRq c)
getScriptRq (Script rqs) serial = case filter ((== serial) . srqSerial) rqs of
    []   -> Nothing
    [rq] -> Just rq


-- ** Constructing scripts

nextFreeSerialScript :: (Script c) -> Ix
nextFreeSerialScript (Script []) = 0
nextFreeSerialScript (Script rqs@(_:_)) = maximum (map srqSerial rqs) + 1

nextFreeSerialContext :: RefCtx -> Ix
nextFreeSerialContext (RefCtx serials _ _) = maybe 0 ((+1) . fst) $ Set.maxView serials

mapSerials :: forall c . (Ix -> Ix) -> (Script c) -> (Script c)
mapSerials f (Script rqs) = Script $ map q rqs
  where
    q :: ScriptRq c -> ScriptRq c
    q rq = rq { srqPath = case srqPath rq of
                            x@(Left e) -> x
                            x@(Right IxRefRoot) -> x
                            (Right (IxRef r)) -> Right (IxRef $ f r) }

-- | Parse body from lines of json code.
readReqBody :: JS.FromJSON c => [LBS] -> Either ST c
readReqBody (LBS.intercalate "\n" -> s) = maybe (Left $ cs s) (Right) $ JS.decode s

emptyReqBody :: Either ST c
emptyReqBody = Left ""

-- | Very basic DSL for writing scripts in a more convenient form
-- (this is work in progress).
newScript :: forall c . [(String, String, Ix -> IxRef -> ScriptRq c)] -> Script c
newScript = Script . f 0 [("/", IxRefRoot)]
  where
    f :: Ix -> [(String, IxRef)] -> [(String, String, Ix -> IxRef -> ScriptRq c)] -> [ScriptRq c]
    f _ _ [] = []
    f i ctx ((label, refstring, x):xs) =
        case lookup refstring ctx of
            Just ixref -> x i ixref : f (i+1) ((label, IxRef i):ctx) xs
            Nothing -> error $ printf "item %i: could not find label %s" i (show label)

newLabelledItem ::
     String
  -> RequestMethod -> String -> Either SBS c -> [(SBS, SBS)] -> [(SBS, SBS)] -> [(SBS, SBS)]
  -> (String, String, Ix -> IxRef -> ScriptRq c)
newLabelledItem label meth refstring body getparams postparams headers =
    (label, refstring, \ ix ref -> ScriptRq ix meth body getparams postparams headers (Right ref))

newItem ::
     RequestMethod -> String -> Either SBS c -> [(SBS, SBS)] -> [(SBS, SBS)] -> [(SBS, SBS)]
  -> (String, String, Ix -> IxRef -> ScriptRq c)
newItem = newLabelledItem ""

sanitizeScript :: JS.FromJSON c => Script c -> Script c
sanitizeScript (Script rqs) = Script $ map sanitizeScriptRq rqs

sanitizeScriptRq :: JS.FromJSON c => ScriptRq c -> ScriptRq c
sanitizeScriptRq r@(ScriptRq _ _ b _ _ _ _) = r { srqBody = sanitizeScriptRqContent b }

sanitizeScriptRqContent :: JS.FromJSON c => Either SBS c -> Either SBS c
sanitizeScriptRqContent (Right c) = Right c
sanitizeScriptRqContent (Left s) = maybe (Left s) (Right) . JS.decode $ cs s


-- ** arbitrary scripts

-- | Generate arbitrary 'Script's.  This is more complicated than
-- 'listOf', since the path refs are context sensitive.  Takes a list
-- of serial numbers of script items in earlier segments.
arbitraryScript :: forall c . (Arbitrary c, Show c)
                => RefCtx -> Maybe [(Int, RequestMethod)] -> Gen (Script c)
arbitraryScript context desiredMethods = Script . reverse . snd <$>
      (listOf (arbitrary :: Gen ()) >>= foldM f (context, []))
  where
    f :: (RefCtx, [ScriptRq c]) -> a -> Gen (RefCtx, [ScriptRq c])
    f (ctx, rqs) _ = do
        rq <- arbitraryScriptRq ctx desiredMethods
        return (scriptContextAdd ctx rq, rq:rqs)

-- | In a given 'RefCtx' and with an (optional) given request method
-- distribution, construct a request with incrementing serial numbers,
-- arbitrary but sound references, arbitrary but sound methods and
-- contents, and empty get and post params and headers.
arbitraryScriptRq :: forall c . (Arbitrary c, Show c)
                  => RefCtx -> Maybe [(Int, RequestMethod)] -> Gen (ScriptRq c)
arbitraryScriptRq ctx desiredMethods = do
      let defaultMethods :: [(Int, RequestMethod)]
          defaultMethods = [ (7, GET)
                           , (5, PUT)
                           , (5, POST)
                           , (2, DELETE)
                           , (1, OPTIONS)
                           , (1, HEAD)
                           ]

      let ix    = nextFreeSerialContext ctx
      ixref    <- fmap Right . QC.elements . Set.toList $ ctxValidPaths ctx
      method   <- QC.frequency . map (second pure) . maybe defaultMethods id $ desiredMethods
      content  <- Right <$> arbitrary

      return $ ScriptRq ix method content [] [] [] ixref


instance (Arbitrary c, Show c, Eq c) => Arbitrary (Script c) where
  arbitrary = arbitraryScript mempty Nothing
  shrink (Script xs) = nub $ dropDanglingReferences . Script <$> shrink xs

instance (Arbitrary c, Show c, Eq c) => Arbitrary (ScriptRq c) where
  arbitrary = arbitraryScriptRq mempty Nothing
  shrink rq = case srqBody rq of
                Left s   -> upd <$> [Left ""]
                Right c  -> upd . Right <$> shrink c
    where
      upd :: Either SBS c -> ScriptRq c
      upd b = rq { srqBody = b }


-- | All scripts must have distinct serial numbers.
prop_arbitraryScriptIx :: (Script c) -> Property
prop_arbitraryScriptIx (Script rqs) = mkprop $ length rqs == (length . nub . map srqSerial $ rqs)

-- | All scripts must not have dangling references.
prop_arbitraryScriptIxRef :: (Script c) -> Property
prop_arbitraryScriptIxRef (Script rqs) = mkprop $ refs `Set.isSubsetOf` ixs
  where
    refs = Set.fromList . catMaybes . map (srqPathSerial . srqPath) $ rqs
    ixs = Set.fromList . map srqSerial $ rqs


-- ** Script contexts

-- | The context of a script at any given point in the request list
-- with respect to references is defined by three sets: (1) All serial
-- numbers in scope, (2) all valid paths (IxRefRoot and the serial
-- numbers of all requests that have created (not modified) a path by
-- method @POST@ or @PUT@), and (3) all deleted paths (method
-- @DELETE@).
data RefCtx = RefCtx
      { ctxSerials :: Set Ix
      , ctxAddedPaths :: Set IxRef
      , ctxRemovedPaths :: Set IxRef
      }
  deriving (Show, Eq, Ord, Generic)

-- | The empty context contains 'IxRefRoot' in the set of added refs.
-- Append crashes if the two sets of serial numbers overlap.  (FIXME:
-- is this comment visible in haddock?  where?)
instance Monoid RefCtx where
    mempty = RefCtx Set.empty (Set.singleton IxRefRoot) Set.empty
    mappend :: RefCtx -> RefCtx -> RefCtx
    mappend (RefCtx serials there gone) (RefCtx serials' there' gone')
           | (Set.null $ Set.intersection serials serials') =
               RefCtx (Set.union serials serials') (Set.union there there') (Set.union gone gone')

-- | The set of all valid references in a given context.
ctxValidPaths :: RefCtx -> Set IxRef
ctxValidPaths ctx = ctxAddedPaths ctx Set.\\ ctxRemovedPaths ctx

scriptContext :: (Show c) => Script c -> RefCtx
scriptContext = foldl scriptContextAdd mempty . fromScript

scriptContextAdd :: (Show c) => RefCtx -> ScriptRq c -> RefCtx
scriptContextAdd ctx@(RefCtx serials created removed) rq =
      case srqPath rq of
        Left _  -> RefCtx serials' created removed
        Right r -> let created' = case srqMethod rq of
                                    POST -> Set.insert r created
                                    PUT -> Set.insert r created  -- (this is an approximation.)
                                    _ -> created
                       removed' = case srqMethod rq of
                                    DELETE -> Set.insert r removed
                                    _ -> removed
                   in RefCtx serials' created' removed'
  where
    s = srqSerial rq
    serials' | not (s `Set.member` serials) = Set.insert s serials
             | True = error $ ppShow ("scriptContextAdd: serial number in use! ", s, ctx, rq)


-- ** Reference maintenance

dropDanglingReferences :: (Show c, Eq c) => Script c -> Script c
dropDanglingReferences = dropDanglingReferences'3


newtype DropDanglingReferences = DropDanglingReferences (Script Int)
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
    good :: (Show c, Eq c) => Script c -> Bool
    good script = dropDanglingReferences'1 script == dropDanglingReferences'2 script &&
                  dropDanglingReferences'2 script == dropDanglingReferences'3 script


-- | Variant 1: Peek into prefix of generated list.
dropDanglingReferences'1 :: forall c . Script c -> Script c
dropDanglingReferences'1 (Script rs) = Script $ catMaybes rs'
  where
    -- Set elements whose path ref points to a deleted item to
    -- 'Nothing'.  This way, we can 'take' the first @i@
    -- elements of rs' during construction and always be sure
    -- not to run into a loop.
    rs' :: [Maybe (ScriptRq c)]
    rs' = map (\ (i, r) ->
                case srqPath r of
                  Left _           -> Just r
                  Right IxRefRoot  -> Just r
                  Right (IxRef ix) -> let context = map srqSerial . catMaybes . take i $ rs' in
                                      if ix `elem` context
                                          then Just r
                                          else Nothing)
              $ zip [0..] rs


-- | Variant 2: 'foldl'.  ('foldr' could make do without the
-- 'reverse'; not sure about space complexity.)
dropDanglingReferences'2 :: forall c . (Show c) => Script c -> Script c
dropDanglingReferences'2 = Script . reverse . snd . foldl f (mempty, []) . fromScript
  where
    f :: (RefCtx, [ScriptRq c]) -> ScriptRq c -> (RefCtx, [ScriptRq c])
    f (ctx, rqs) rq = if maybe False (`Set.member` ctxAddedPaths ctx) $ srqPathRef (srqPath rq)
                          then (scriptContextAdd ctx rq, rq:rqs)
                          else (ctx, rqs)


-- | Variant 3: tail recursion.  This is my favorite :)
dropDanglingReferences'3 :: forall c . (Show c) => Script c -> Script c
dropDanglingReferences'3 (Script rqs) = Script $ f mempty rqs
  where
    f :: RefCtx -> [ScriptRq c] -> [ScriptRq c]
    f context [] = []
    f context (rq:rqs) = if maybe False (`Set.member` ctxAddedPaths context) $ srqPathRef (srqPath rq)
                            then rq  : f (mappend context (scriptContextAdd mempty rq)) rqs
                            else f context rqs



-- * Stories

-- | A 'Story' is an interpreted 'Script'.  Story items (or events, or
-- actions) consist of a 'ScriptRq' and an HTTP response.  The are
-- arranged in a list in the same order as in the 'Script', i.e. last
-- item was played last.  See 'runScript' and 'runScript''.
newtype Story c = Story { fromStory :: [StoryItem c] }

type StoryItem c = (ScriptRq c, Maybe (Response LBS))

instance (Show c) => Show (Story c) where
  show (Story [])       = "(empty Story)\n"
  show (Story xs@(_:_)) = ("Story\n  " <>) . intercalate "\n  " . lines . concat . map f $ xs
    where
      f :: StoryItem c -> String
      f (rq, rsp) = ">>>>>\n" <> ppShow rq <> "\n"
                 <> case rsp of
                      Just rsp_ -> "<<<<<\n" <> show rsp_ <> "\n" <> cs (rspBody rsp_) <> "\n"
                      Nothing -> "<<<<<\n(skipped)\n"

instance Monoid (Story c) where
  mappend (Story xs) (Story ys) = Story $ xs ++ ys
  mempty = Story []


getStoryItem :: Story c -> Ix -> Either (StoryError c) (StoryItem c)
getStoryItem (Story story) serial =
    case filter ((== serial) . srqSerial . fst) story of
        [result@(rq, rsp)] -> case (rsp, rspCode <$> rsp) of
                                 (Just rsp_, Just (2, _, _))  -> Right result
                                 (_, Just code)               -> Left (StoryErrorHTTP code)
                                 (_, Nothing)                 -> Left (StoryErrorSkipped rq)
        [] -> Left StoryErrorSerialNotFound


data StoryError c =
      StoryErrorSerialNotFound
    | StoryErrorHTTP ResponseCode
    | StoryErrorSkipped (ScriptRq c)
    | StoryErrorJSON
  deriving (Show, Eq)



-- * reference reduction

-- | Headers and body of a 'ScriptRq' may contain (sub-)strings of the
-- form @___SCRIPT_REF___<i>@, or @___SCRIPT_REF___@, which index into
-- earlier 'Script' items (the latter into 'IxRefRoot').  This
-- function finds those, calls a transformer on each occurrance, and
-- substitutes it by the resulting string.  It accepts a 'RefCtx' so
-- that references to non-existing items can be answered with a
-- comprehensive error message.  The first argument states whether
-- surrounding quotes (double or single) should be matched away
-- together with the reference string.
reduceRefs :: Bool -> RefCtx -> (IxRef -> LBS) -> LBS -> LBS
reduceRefs quoted ctx rewrite js =
      if null badrefs
        then replaceRegexAll js pattern (fmap rewrite . extract quoted)
        else error $ "reduceRefs: illegal references: " ++ show badrefs
  where
    pattern :: SBS = q <> "___SCRIPT_REF___(\\d+)?" <> q
      where q = if quoted then "(\"|\')" else ""

    extract :: Bool -> [(LBS, (MatchOffset, MatchLength))] -> Maybe IxRef
    extract False [_, (readMay . cs . fst) -> Just i]                         = Just $ IxRef i
    extract False [_, ("", (-1, 0))]                                          = Just $ IxRefRoot
    extract True  [_, ("'", _),  (readMay . cs . fst) -> Just i, ("'", _)]    = Just $ IxRef i
    extract True  [_, ("'", _),  ("", (-1, 0)),                  ("'", _)]    = Just $ IxRefRoot
    extract True  [_, ("\"", _), (readMay . cs . fst) -> Just i, ("\"", _)]   = Just $ IxRef i
    extract True  [_, ("\"", _), ("", (-1, 0)),                  ("\"", _)]   = Just $ IxRefRoot
    extract _     _                                                           = Nothing

    refs :: Set Ix
         = Set.fromList
         . catMaybes . map fromIxRef
         . catMaybes . map (extract quoted)
         $ js =~++ pattern

    badrefs :: [Int]
         = Set.toList $ refs Set.\\ ctxSerials ctx


-- | (Just a quick in-place unit test for 'reduceRefs'.)
test_reduceRefs :: Bool
test_reduceRefs = f True == ["IxRefRoot","IxRef 0"] && f False == ["IxRefRoot","IxRef 0"]
  where
    f quote = map (reduceRefs quote ctx (cs . show) . scriptRefToSBS quote) refs
    ctx = RefCtx (Set.fromList [0]) (Set.fromList refs) Set.empty
    refs = [IxRefRoot, IxRef 0]


-- | Render 'IxRef' into a string suitable for being found by
-- 'reduceRefs'.  The boolean flag is for double-quotation (yes
-- means double-quotes).
scriptRefToSBS :: Bool -> IxRef -> LBS
scriptRefToSBS False IxRefRoot = "___SCRIPT_REF___"
scriptRefToSBS False (IxRef i) = "___SCRIPT_REF___" <> cs (show i)
scriptRefToSBS True  IxRefRoot = "\"___SCRIPT_REF___\""
scriptRefToSBS True  (IxRef i) = "\"___SCRIPT_REF___" <> cs (show i) <> "\""


-- | Dynamic 'reduceRefs', as needed for 'Story'.  (See 'reduceRefsPy'
-- for a static variant.)
reduceRefsDyn :: forall c . (Show c, JS.FromJSON c, JS.ToJSON c)
         => Bool -> URI -> (StoryItem c -> URI) -> Story c
         -> SBS -> SBS
reduceRefsDyn quoted rootPath constructPath story = cs . reduceRefs quoted ctx (qu . ru . lu) . cs
  where
    script :: Script c = Script . map fst $ fromStory story
    ctx    :: RefCtx   = scriptContext script

    lu :: IxRef -> URI   -- lookup xref
    lu IxRefRoot = rootPath
    lu (IxRef i) = case filter ((== i) . srqSerial . fst) $ fromStory story of
                     [item] -> constructPath item

    ru :: URI -> LBS     -- render uri
    ru = cs . uriPath

    qu :: LBS -> LBS     -- re-quote (if applicable)
    qu = if quoted then cs . show else id


reduceRefsDynAssoc :: forall c . (Show c, JS.FromJSON c, JS.ToJSON c)
         => URI -> (StoryItem c -> URI) -> Story c
         -> [(SBS, SBS)] -> [(SBS, SBS)]
reduceRefsDynAssoc root construct story = map (first f . second f)
  where f = reduceRefsDyn False root construct story


reduceRefsDynBody :: forall c . (Show c, JS.FromJSON c, JS.ToJSON c)
         => URI -> (StoryItem c -> URI) -> Story c
         -> Either SBS c -> Either SBS c
reduceRefsDynBody root construct story (Left s) =
    Left (reduceRefsDyn True root construct story s)
reduceRefsDynBody root construct story (Right c) =
    sanitizeScriptRqContent . Left . reduceRefsDyn True root construct story . cs $ JS.encode c



-- * interpreter

-- | Run a 'Script' and return a list of pairs of 'ScriptRq' and HTTP
-- responses with serial.  Requests on paths missing due to earlier
-- errors are skipped (response is 'Nothing').  See 'Script' for
-- properties of serial numbers.
runScript :: forall c . (Show c, JS.FromJSON c, JS.ToJSON c)
          => Bool -> URI -> (StoryItem c -> URI) -> Script c -> IO (Story c)
runScript verbose rootPath constructPath (Script rs) = foldM f (Story []) rs
  where
    f :: Story c -> ScriptRq c -> IO (Story c)
    f story rq@(ScriptRq serial method body getparams postparams headers pathref) = do
            let pathMay :: Maybe URI
                        = case pathref of
                            Right (IxRef ix) ->
                              case getStoryItem story ix of
                                 Right item                     -> Just $ constructPath item
                                 Left StoryErrorSerialNotFound  -> error $ "runScript: dangling pathref: " ++ ppShow (pathref, story)
                                 Left (StoryErrorHTTP _)        -> Nothing
                                 Left (StoryErrorSkipped _)     -> Nothing
                            Right IxRefRoot ->
                              Just rootPath
                            Left uri ->
                              Just uri

            case pathMay of
              Just path -> do
                let body'       = reduceRefsDynBody rootPath constructPath story body
                    getparams'  = evl getparams
                    postparams' = evl postparams
                    headers'    = evl headers
                    evl         = reduceRefsDynAssoc rootPath constructPath story

                response <- performReq verbose method path getparams' postparams' headers' body'
                return $ story <> Story [(rq, Just response)]
              Nothing -> do
                return $ story <> Story [(rq, Nothing)]


-- | Clear entire database on server, then run 'runScript'.
runScript' :: forall c . (Show c, JS.FromJSON c, JS.ToJSON c)
           => Bool -> URI -> (StoryItem c -> URI) -> Script c -> IO (Story c)
runScript' v r c s = clearDB r >> runScript v r c s


-- | Clear entire database on server.
clearDB :: URI -> IO ()
clearDB r = performReqEmptyBody False GET r' [] [] [] >> return ()
  where
    r' = r { uriPath = uriPath r <> "/admin/reset-db" }



-- * compile 'Script' object to python

scriptToPyFile :: (JS.ToJSON c, Show c) => Script c -> FilePath -> IO ()
scriptToPyFile script filepath = SBS.writeFile filepath . SBS.intercalate "\n" . scriptToPySBS $ script

scriptToPySBS :: (JS.ToJSON c, Show c) => Script c -> [SBS]
scriptToPySBS (Script rqs) = pyHeader ++ rqs' ++ ["", "print 'success!'", ""]
  where
    rqs' :: [SBS]
    rqs' = concatMap (uncurry scriptRqToPy) $ zip ctxs rqs

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
scriptRqToPy :: (JS.ToJSON c) => RefCtx -> ScriptRq c -> [SBS]
scriptRqToPy ctx (ScriptRq x m b [] [] hs r) =
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

    showRef :: Either URI IxRef -> SBS
    showRef (Right (IxRef i))  = "resp_" <> cs (show i) <> ".json()['path']"
    showRef (Right IxRefRoot)  = "rootpath"
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
        f IxRefRoot = "rootpath"
        f (IxRef i) = "resp_" <> cs (show i) <> ".json()['path']"



-- * quickcheck unit test store helpers

-- | List all unit test cases in the DB, together with their filenames.
readStore :: forall a c . (Show a, Typeable a, Cereal.Serialize a)
        => (a -> Script c) -> IO [(FilePath, Script c)]
readStore toScript = do
    let path = (storeRoot </> show (typeOf (undefined :: a)))
    filenames <- filter (not . (`elem` [".", ".."])) <$> getDirectoryContents path
    mapM (\ filename -> (path </> filename,) . toScript <$> decodeConfidently (path </> filename)) filenames


-- | Given a function that transforms a given type into a 'Script',
-- compile all test cases of that type into Haskell and Python.
compileStore :: forall a c . (Show a, Typeable a, Cereal.Serialize a, Show c, JS.ToJSON c)
        => (a -> Script c) -> IO ()
compileStore toScript = readStore toScript >>= mapM_ (compileTestCase toScript)


-- | Test cases that are newtype-wrapped 'Script's can be compiled to
-- both Haskell data (for inspection and modification) and python (for
-- convenience and inclusion in external test workflows).
compileTestCase :: forall a c . (Show a, Typeable a, Cereal.Serialize a, Show c, JS.ToJSON c)
        => (a -> Script c) -> (FilePath, Script c) -> IO ()
compileTestCase _ (filename, testData) = do
    writeFile (dropExtension filename <.> "hs") $ ppShow testData
    scriptToPyFile testData (dropExtension filename <.> "py")



-- * more helpers

getScriptRqContent :: ScriptRq c -> Maybe c
getScriptRqContent (ScriptRq _ _ (Right body) _ _ _ _) = Just body
getScriptRqContent _ = Nothing


getScriptContent :: Script c -> Ix -> Maybe c
getScriptContent (Script rqs) serial = case filter ((== serial) . srqSerial) rqs of
    []   -> Nothing
    [rq] -> getScriptRqContent rq


getStoryRsp :: (JS.FromJSON c) => Story c -> Ix -> Either (StoryError c) c
getStoryRsp story serial =
    getStoryItem story serial >>= \ (_, rsp) ->
        case rsp >>= JS.decode . cs . rspBody of
            Just c -> Right c
            Nothing -> Left StoryErrorJSON


-- | ratio of 200 responses vs. all others.
errorRate :: Story c -> Double
errorRate (Story story) = fromIntegral (length errors) / fromIntegral (length story)
  where
    errors = filter (\ (_, rsp) -> (rspCode <$> rsp) /= Just (2, 0, 0)) story


-- | ratio of requests not sent due to earlier errors vs. all others.
skipRate :: Story c -> Double
skipRate (Story story) = fromIntegral (length skipped) / fromIntegral (length story)
  where
    skipped = filter (isNothing . snd) story
