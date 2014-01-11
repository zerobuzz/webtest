{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

{- | The foundation of this module was written by Sönke Hahn [1].  It
provides two ways of keeping track of failed test cases in the file
system, and re-playing them as traditional unit tests.

The store functionality uses the cereal package instead of 'Show'
instances for serialization.  The stash functionality is an orthogonal
approach that explicitly saves counter-examples away in quickcheck
failure call backs.

Instead of being called implicitly by the quickCheck* function family
like the store, the stash has to be called explicitly in the property
with its own 'whenFail' clause.  This is a bit more awkward, but a bit
less of a stretch for the haskell type class mechanics: properties
don't have to have type 'testcase -> property', but just 'property'.

Using @stash@, a typical debugging session after a proprety has failed
may look as follows:

> -- First, find counter-examples for the property.  (See code of
> -- 'runprop_ObjectsDontDisappear' for an example of how to stash
> -- counter-examples.)  call this a few times until you see a
> -- counter-example that you want to work with.
> a = quickCheck prop_ObjectsDontDisappear :: IO ()
>
> -- pull in the entire stash.
> b = stashGet :: IO [ObjectsDontDisappear]
>
> -- print the entire stash line-by-line.
> c = b >>= mapM_ print . zip [0..]
>
> -- pick the one you liked.
> q = (!! 7) <$> stashGet :: IO ObjectsDontDisappear
>
> -- take a closer look.
> p = q >>= putStrLn . ppShow
>
> -- run it again.
> v :: IO Story
> v = q >>= \ (ObjectsDontDisappear (script, _, _)) -> runScript' False "/adhocracy" script
>
> -- pass it to runprop_*.  (repeat this until you don't get an error
> -- any more.  then run a again.)
> w :: IO ()
> w = q >>= quickCheck . runprop_ObjectsDontDisappear


[1] http://patch-tag.com/r/shahn/QuickCheckStore

-}
module Test.QuickCheck.Store where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Monoid
import Data.Time
import Data.Typeable
import Safe
import System.Cmd
import System.Directory
import System.Exit
import System.FilePath
import System.Locale
import System.Time
import Test.QuickCheck
import Text.Printf

import qualified Data.ByteString as SBS
import qualified Data.Serialize as Cereal



-- | path to stored values (will be configurable via cli and config file)
storeRoot :: FilePath
storeRoot = ".quickcheck-store"


-- | like quickCheckStoreWith, but with stdArgs
quickCheckResultStore ::
    forall testData property .
    (Eq testData, Typeable testData, Cereal.Serialize testData, Show testData,
     Arbitrary testData, Testable property) =>
    (testData -> property) -> IO Result
quickCheckResultStore = quickCheckWithResultStore stdArgs


-- | Runs a given property with arbitrary test data.  Replays data
-- that produced failures in past runs.  Replay test data is stored in
-- @'storeRoot' </> typeOf testData </> timestamp@.
quickCheckWithResultStore ::
    forall testData property .
    (Eq testData, Typeable testData, Cereal.Serialize testData, Show testData,
     Arbitrary testData, Testable property) =>
    Args -> (testData -> property) -> IO Result
quickCheckWithResultStore args prop = do
    let label = show (typeOf (undefined :: testData))
    storedTestCases <- loadTestCases label

    unitTests :: Maybe Result
       <- if null storedTestCases
                then return Nothing
                else Just <$> quickCheckWithResult args{maxSuccess = 1}
                        (foldr1 (.&&.)
                            (map (\ (file, t) -> printTestCase (label </> file ++ ": " ++ show t) (prop t))
                                storedTestCases))

    -- FIXME: output should make it more explicit when unit tests are
    -- pulled as opposed to when arbitrary test cases are generated.
    -- it can get very confusing during debugging of the test case
    -- generators.

    case unitTests of
        -- no stored tests - try arbitrary
        Nothing -> testArbitrary args prop (map snd storedTestCases)

        -- all stored tests successful - try arbitrary
        Just Success{} -> testArbitrary args prop (map snd storedTestCases)

        -- some stored tests failed - propagate result
        Just noSuccess -> return noSuccess


-- | test new arbitrary values
testArbitrary :: (Typeable testData, Cereal.Serialize testData, Eq testData, Show testData,
                  Arbitrary testData, Testable property) =>
    Args -> (testData -> property) -> [testData] -> IO Result
testArbitrary args prop storedTestCases =
    quickCheckWithResult args $ \ testData ->
        not (testData `elem` storedTestCases) ==>
            whenFail (storeTestData testData)
                (prop testData)


-- | test previously failed values
loadTestCases :: Cereal.Serialize a => FilePath -> IO [(FilePath, a)]
loadTestCases label = do
    storeExists <- doesDirectoryExist (storeRoot </> label)
    if storeExists then do
        testFiles <- map ((storeRoot </> label) </>) <$>
            reverse <$> sort <$>
            filter ((== ".bin") . takeExtension) <$>
            getDirectoryContents (storeRoot </> label)
        testCases <- mapM decodeConfidently testFiles
        return $ zip (map takeBaseName testFiles) testCases
      else
        return []


decodeConfidently :: Cereal.Serialize a => FilePath -> IO a
decodeConfidently fn = let crash msg = error $ "loadTestCases: " ++ show (msg, fn)
                       in either (crash fn) id . Cereal.decode <$> SBS.readFile fn


storeTestData :: (Typeable a, Cereal.Serialize a) => a -> IO ()
storeTestData testData = do
    let dir :: String = storeRoot </> show (typeOf testData)
    createDirectoryIfMissing True dir
    file <- (dir </>) . (<.> "bin") <$> timestamp
    let testDataS = Cereal.encode testData
    testDataS `seq` SBS.writeFile file testDataS
  where
    timestamp :: IO String
    timestamp = formatTime defaultTimeLocale "%Z-%Y-%m-%d-%H-%M-%S-%q" <$> (getCurrentTime :: IO UTCTime)



-- * example

example = do
    quickCheckWithResultStore stdArgs exampleProperty

exampleProperty :: Int -> Bool
exampleProperty = not . (> 10)

main = example



-- * quickcheck stash

-- | (Must not contain whitespace or quotes or double-quotes.)
stashPath :: IO FilePath
stashPath = pure ".quickcheck-stash"

stashClear :: IO ()
stashClear = stashPath >>= system . ("rm -rf " <>) . show >>= \ ExitSuccess -> return ()

stashPut :: Cereal.Serialize a => a -> IO ()
stashPut x = do
    stashpath <- stashPath
    system $ "mkdir -p " <> show stashpath
    filepath <- (stashpath </>) . printf "%3.3i.bin" . length <$> getDirectoryContents stashpath
    SBS.writeFile filepath $ Cereal.encode x

-- | Retrieve a list of all items passed to 'stashPut' since the last
-- 'stashClear'.  'stashPut' must not be passed objects of different
-- types between two calls to 'stashClear' if you want 'stashGet' to
-- function.
stashGet :: Cereal.Serialize a => IO [a]
stashGet = do
    stashpath <- stashPath
    filepaths <- sort . filter ((== ".bin") . takeExtension) <$> getDirectoryContents stashpath
    forM filepaths $ \ filepath ->
        either (error . ("stashGet: " <>)) id . Cereal.decode <$> (SBS.readFile $ stashpath </> filepath)
