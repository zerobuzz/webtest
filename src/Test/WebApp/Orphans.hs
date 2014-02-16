{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE NoImplicitPrelude                        #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE ViewPatterns                             #-}

{-# OPTIONS -fwarn-unused-imports -fwarn-incomplete-patterns #-}

{-| Some instances for types from other packages.  You can avoid any
orphan problems by not importing this module, but instead copying the
orphans you need into your own code.  (A cleaner way would be to wrap
all types that we need instances for with newtypes, that that would
also be more tedious.)  -}
module Test.WebApp.Orphans where

import Control.Applicative
import Control.Monad hiding (mapM, forM)
import Data.Function
import Data.HashMap.Strict (HashMap (..))
import Data.Int
import Data.List
import Data.Map (Map)
import Data.String.Conversions
import Data.Traversable
import Network.HTTP
import Network.URI
import Prelude hiding (mapM)
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck as Q

import qualified Data.Aeson as JS
import qualified Data.Attoparsec.Number
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map hiding (Map)
import qualified Data.Serialize as Cereal
import qualified Data.Vector as V
import qualified Snap.Core as Snap

import Test.WebApp.Arbitrary


-- * Arbitraries

instance Arbitrary Data.Attoparsec.Number.Number where
  arbitrary = arbitraryNumber 1000
  shrink = map fromRational . shrink . toRational

instance (Arbitrary b) => Arbitrary (HashMap ST b) where
  arbitrary = HashMap.fromList <$> arbitrary
  shrink = map HashMap.fromList . shrink . HashMap.toList

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (Map a b) where
  arbitrary = Map.fromList <$> arbitrary
  shrink = map Map.fromList . shrink . Map.toList

instance Arbitrary SBS where
  arbitrary = cs <$> (arbitrary :: Gen String)
  shrink = map cs . shrink . (cs :: SBS -> String)

instance Arbitrary ST where
  arbitrary = cs <$> (arbitrary :: Gen String)
  shrink = map cs . shrink . (cs :: ST -> String)

-- | Generate strings that are random, but from a small set so that
-- value repetition is more likely.
arbitrarySomeST :: Gen ST
arbitrarySomeST = variant2
  where
    variant1 :: Gen ST = Q.elements $ "" : cs (replicate 3000 '@') : unsafePerformIO (sample' arbitrary)
    variant2 :: Gen ST = Q.elements $ "" : take 3 readableStringCollection

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary
  shrink = map V.fromList . shrink . V.toList

instance Arbitrary Snap.Method where
  arbitrary = oneof $ map pure [Snap.GET, Snap.HEAD, Snap.POST, Snap.PUT, Snap.DELETE,
                                Snap.TRACE, Snap.OPTIONS, Snap.CONNECT, Snap.PATCH,
                                Snap.Method "custom"]
  shrink _ = []

instance Arbitrary RequestMethod where
  arbitrary = oneof $ map pure $ GET :
                                 HEAD :
                                 POST :
                                 PUT :
                                 DELETE :
                                 TRACE :
                                 OPTIONS :
                                 CONNECT :
                                 Custom "custom" :
                                 []
  shrink _ = []

instance Arbitrary JS.Value where
  arbitrary = frequency $
      (30, JS.Object <$> arbitrary') :
      (20, JS.Array <$> arbitrary') :
      (3, JS.String <$> arbitrarySomeST) :
      (3, JS.Number <$> arbitraryNumber 1000) :
      (3, JS.Bool <$> arbitrary') :
      (3, pure JS.Null) :
      []

  shrink (JS.Object x) = JS.Object <$> shrink x
  shrink (JS.Array x) = JS.Array <$> shrink x
  shrink (JS.String x) = JS.String <$> shrink x
  shrink (JS.Number i) = map (JS.Number . fromRational) . shrink . toRational $ i
  shrink (JS.Bool x) = []
  shrink JS.Null = []



-- * Fuzz

-- (Some Fuzz instances call arbitrary, and thus depend on the orphan
-- instances in this module.  Those that don't are defined in sibling
-- module Arbitrary.)

instance Fuzz Data.Attoparsec.Number.Number where
  fuzz b = frequency [(14, pure b), (3, arbitrary)]

{-
-- | A very simple JS.Value instance as a proof of concept.
instance Fuzz JS.Value where
  fuzz js = frequency [(13, f js), (3, arbitrary)]
    where
      f (JS.Object m)     = JS.Object <$> fuzz m
      f (JS.Array v)      = JS.Array <$> fuzz v
      f (JS.String s)     = JS.String <$> fuzz s
      f (JS.Number n)     = JS.Number <$> fuzz n
      f (JS.Bool b)       = JS.Bool <$> fuzz b
      f JS.Null           = pure JS.Null
-}

instance Fuzz JS.Value where
  fuzz = tweakType
    where
      tweakType :: JS.Value -> Gen JS.Value
      tweakType (JS.Object m) = JS.Object <$> oneof [ tweak (\ (k, v) -> (,v) <$> fuzz k) m
                                                    , tweak (\ (k, v) -> (k,) <$> fuzz v) m
                                                    ]
        where
          -- FIXME: missing:
          --   . new keys.
          --   . drop keys.
          --   . flip values.
          --   . empty map.

          tweak :: ((ST, JS.Value) -> Gen (ST, JS.Value)) -> HashMap ST JS.Value -> Gen (HashMap ST JS.Value)
          tweak g = fmap HashMap.fromList . f . HashMap.toList
            where
              f :: [(ST, JS.Value)] -> Gen [(ST, JS.Value)]
              f l = do
                i <- choose (0, length l - 1)
                let (xs, y:ys) = splitAt i l
                y' <- g y
                return $ xs ++ [y'] ++ ys

      tweakType (JS.Array a) = JS.Array <$> (choose (0, V.length a - 1) >>= tweakVector a)
        where
          -- FIXME: missing:
          --  . flip two entries.
          --  . insert an entry.
          --  . drop an entry.
          --  . empty array.

          tweakVector :: V.Vector JS.Value -> Int -> Gen (V.Vector JS.Value)
          tweakVector v i = (\ x' -> V.update v (V.fromList [(i, x')])) <$> tweakType (v V.! i)

      tweakType (JS.String s) = (arbitrary :: Gen Double) >>= baseType s
      tweakType (JS.Number i) = arbitrary >>= (`baseType` (fromRational $ toRational i))
      tweakType (JS.Bool b) = JS.Bool <$> elements [minBound..]
      tweakType JS.Null = return JS.Null

      baseType :: ST -> Double -> Gen JS.Value
      baseType s i = do
        ss :: [ST] <- nub <$> (arbitrary >>= Data.Traversable.sequence . (`replicate` (fuzz s)))
        elements $
          (JS.Number <$> [ fromIntegral (minBound :: Data.Int.Int16)
                         , fromIntegral (maxBound :: Data.Int.Int16)
                         , fromIntegral (minBound :: Data.Int.Int32)
                         , fromIntegral (maxBound :: Data.Int.Int32)
                         , fromIntegral (minBound :: Data.Int.Int64)
                         , fromIntegral (maxBound :: Data.Int.Int64)
                         , 0, 1e-5, -1e-5, 1e-25, -1e-25
                         , (fromRational $ toRational i)+1e-5, (fromRational $ toRational i)-1e-5
                         ]) ++
          (JS.String <$> (s : ss))



-- * serialization

-- | "Network.HTTP" request methods.  The Custom "String" case is not
-- covered.
instance Cereal.Serialize RequestMethod where
  put = Cereal.put . f
    where
      f :: RequestMethod -> String
      f OPTIONS    = "OPTIONS"
      f HEAD       = "HEAD"
      f GET        = "GET"
      f PUT        = "PUT"
      f POST       = "POST"
      f DELETE     = "DELETE"
      f TRACE      = "TRACE"
      f CONNECT    = "CONNECT"
      f oops       = error $ "instance Serialize RequestMethod: unsuppoerted constructor: " ++ show oops

  get = Cereal.get >>= f
    where
      f :: String -> Cereal.Get RequestMethod
      f "OPTIONS"     = return OPTIONS
      f "HEAD"        = return HEAD
      f "GET"         = return GET
      f "PUT"         = return PUT
      f "POST"        = return POST
      f "DELETE"      = return DELETE
      f "TRACE"       = return TRACE
      f "CONNECT"     = return CONNECT
      f _             = mzero

instance Cereal.Serialize URI where
  put (URI uriScheme
           uriAuthority
           uriPath
           uriQuery
           uriFragment) = do
      Cereal.put uriScheme
      Cereal.put uriAuthority
      Cereal.put uriPath
      Cereal.put uriQuery
      Cereal.put uriFragment

  get = URI <$> Cereal.get <*> Cereal.get <*> Cereal.get <*> Cereal.get <*> Cereal.get

instance Cereal.Serialize URIAuth where
  put (URIAuth uriUserInfo
               uriRegName
               uriPort) = do
      Cereal.put uriUserInfo
      Cereal.put uriRegName
      Cereal.put uriPort

  get = URIAuth <$> Cereal.get <*> Cereal.get <*> Cereal.get
