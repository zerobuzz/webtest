{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# OPTIONS -fwarn-unused-imports -fwarn-incomplete-patterns #-}

module Test.WebApp.HTTP.Util
where

import Control.Monad hiding (mapM, forM)
import Data.Function
import Data.List
import Data.Maybe
import Data.String.Conversions
import Network.Stream
import Network.URI
import Prelude hiding (mapM)

import qualified Data.Aeson as JS
import qualified Data.Aeson.Encode.Pretty as JS
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP as NH



injectBody :: JS.ToJSON v => NH.Request LBS -> Either ST v -> NH.Request LBS
injectBody req bodyE =
    let bodyS :: LBS = either cs JS.encodePretty bodyE in
    NH.replaceHeader NH.HdrContentType "application/json" $
    NH.replaceHeader NH.HdrContentLength (show $ LBS.length bodyS) $
    req { NH.rqBody = bodyS }


-- | Send request to server, collect response, and optionally dump
-- request and response to stdout.
performReq :: JS.ToJSON w => Bool -> NH.RequestMethod -> URI -> [(ST, ST)] -> Either ST w -> IO (NH.Response LBS)
performReq verbose method path headers bodyE = do
    let req_0 = injectBody (NH.mkRequest method path) bodyE
        req = foldl (\ req (k, v) -> NH.replaceHeader (NH.HdrCustom (cs k)) (cs v) req) req_0 headers

    when verbose $ do
        putStr "\n[=[REQUEST]============================================================\n"
        putStr $ show req <> "\n"
        putStr "\n-----------------------------------------------------------------------\n"
        LBS.putStr $ NH.rqBody req <> "\n"
        putStr "\n------------------------------------------------------------[REQUEST]-]\n"

    responseE :: Either ConnError (NH.Response LBS)
             <- NH.simpleHTTP req

    case responseE of
        (Left err)      -> error $ show err
        (Right response) -> do
            when verbose $ do
                putStr "\n[-[RESPONSE]-----------------------------------------------------------\n"
                putStr $ show response <> "\n"
                putStr "\n----------------------------------------------------------------------\n"
                LBS.putStr $ NH.rspBody response <> "\n"
                v <- case NH.rspCode response of
                         code@(i, _, _) -> when (i /= 2) . putStr $
                             " *** failed with response code: " <> show code <> "\n"
                putStr "\n-----------------------------------------------------------[RESPONSE]-]\n"

            return response


performReqEmptyBody :: Bool -> NH.RequestMethod -> URI -> [(ST, ST)] -> IO (NH.Response LBS)
performReqEmptyBody verbose method path headers = performReq verbose method path headers (Left "" :: Either ST JS.Value)


processResponse :: JS.FromJSON v => Bool -> NH.Response LBS -> Maybe v
processResponse verbose_ response =
    case NH.rspCode response of
          (2, _, _) -> JS.decode . cs . NH.rspBody $ response
          code      -> Nothing


-- | Process response, force response code 2xx and remove 'Maybe' wrapper.
forceResponse :: JS.FromJSON v => Bool -> NH.Response LBS -> v
forceResponse verbose_ response =
    maybe (error "giving up") id $ processResponse verbose_ response


-- | Process response, force response code 2xx and drop result entirely.
assertResponse :: Bool -> NH.Response LBS -> ()
assertResponse verbose_ response =
    maybe (error "giving up") (const ()) $
      (processResponse verbose_ response :: Maybe JS.Value)


-- | Process response, force response code /= 2xx and drop result
-- entirely.  ('processResponse' above could also return an either
-- error that we can process here.)
assertError :: Bool -> NH.Response LBS -> ()
assertError verbose_ response =
    maybe () (const $ error "giving up") $
      (processResponse verbose_ response :: Maybe JS.Value)
