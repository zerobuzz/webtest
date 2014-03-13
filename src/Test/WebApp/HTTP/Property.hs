{-# LANGUAGE BangPatterns                             #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving               #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE NoImplicitPrelude                        #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE PackageImports                           #-}
{-# LANGUAGE PatternGuards                            #-}
{-# LANGUAGE QuasiQuotes                              #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE RecordWildCards                          #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE StandaloneDeriving                       #-}
{-# LANGUAGE TemplateHaskell                          #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeSynonymInstances                     #-}
{-# LANGUAGE ViewPatterns                             #-}

{-# OPTIONS -fwarn-unused-imports -fwarn-incomplete-patterns #-}

module Test.WebApp.HTTP.Property
where

import Control.Monad hiding (mapM, forM)
import Data.Function
import Data.Maybe
import Data.String.Conversions
import Network.HTTP
import Network.URI
import Prelude hiding (mapM)
import Test.QuickCheck as Q
import Test.QuickCheck.Property

import qualified Data.Aeson as JS
import qualified Data.Serialize as Cereal

import Test.WebApp.HTTP.Util



-- | JSON serialization stability.  Note that 'JS.decode' only works
-- on top-level.  The classification makes it explicit if that is your
-- issue.
propFromToJSON :: (Eq a, JS.FromJSON a, JS.ToJSON a) => a -> Property
propFromToJSON v = if topLevel j
                     then label "ok" $ JS.decode (JS.encode j) == Just j
                     else label "json value not top-level" False
  where
    s :: LBS
    s = JS.encode v

    j :: JS.Value
    j = JS.toJSON v

    topLevel :: JS.Value -> Bool
    topLevel j@(JS.Object _) = True
    topLevel j@(JS.Array _) = True
    topLevel _ = False


-- | Cereal serialization stability.
propCereal :: (Eq a, Cereal.Serialize a) => a -> Bool
propCereal x  = case Cereal.decode $ Cereal.encode x of
                  Left _ -> False
                  Right x' -> x == x'


-- | Check if server survives white HTTP noise without 5xx responses.
propHttpWhiteNoise :: JS.ToJSON v => RequestMethod -> URI -> Either SBS v -> Q.Property
propHttpWhiteNoise method path contentE = morallyDubiousIOProperty $ do
    response <- performReq False method path [] [] [] contentE
    return $ case rspCode response of
        (5, _, _) -> False
        _         -> True
