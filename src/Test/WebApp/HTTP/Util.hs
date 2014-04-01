{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# OPTIONS -fwarn-unused-imports #-}  -- -fno-warn-incomplete-patterns

module Test.WebApp.HTTP.Util
where

import Control.Monad hiding (mapM, forM)
import Data.Function
import Data.List
import Data.Maybe
import Data.String.Conversions
import Network.HTTP
import Network.Stream
import Network.URI
import Prelude hiding (mapM)
import System.FilePath

import qualified Data.Aeson as JS
import qualified Data.Aeson.Encode.Pretty as JS
import qualified Data.ByteString.Lazy as LBS



-- | 'encodePretty' with more reasonable defaults.
prettyJS :: JS.ToJSON a => a -> LBS
prettyJS = JS.encodePretty' (JS.defConfig { JS.confCompare = compare })


-- | If request body is json, pretty-print it.
prettyReqBody :: Request LBS -> Request LBS
prettyReqBody r =
    case JS.decode $ rqBody r of
        (Just (v :: JS.Value)) -> r { rqBody = prettyJS v }
        Nothing -> r


-- | If response body is json, pretty-print it.
prettyRespBody :: Response LBS -> Response LBS
prettyRespBody r =
    case JS.decode $ rspBody r of
        (Just (v :: JS.Value)) -> r { rspBody = prettyJS v }
        Nothing -> r


-- | Cheating around 'Custom' constructor to get 'Enum' behavior.
requestMethods :: [RequestMethod]
requestMethods = [HEAD, PUT, GET, POST, DELETE, OPTIONS, TRACE, CONNECT]


-- | Compile a URI from path string with defaults suitable for testing.
mkURI :: SBS -> URI
mkURI = mkURI' "localhost" 8000 "/"


-- | Compile a URI from testing parameters.
mkURI' :: SBS -> Int -> SBS -> SBS -> URI
mkURI' serverHost serverPort restRoot path = nullURI {
    uriScheme     = "http:",
    uriAuthority  = Just (URIAuth "" (cs serverHost) (':': show serverPort)),
    uriPath       = (cs restRoot) </>
                    case cs path of
                      ('/':x) -> x
                      x       -> x
  }


injectBody :: JS.ToJSON v => Request LBS -> Either SBS v -> Request LBS
injectBody req bodyE =
    let bodyS :: LBS = either cs JS.encodePretty bodyE in
    replaceHeader HdrContentType "application/json" $
    replaceHeader HdrContentLength (show $ LBS.length bodyS) $
    req { rqBody = bodyS }


-- | Send request to server, collect response, and optionally dump
-- request and response to stdout.  FIXME: currently, get and post
-- params are not supported (the first and second assoc map must be
-- []).
performReq :: JS.ToJSON w
           => Bool
           -> RequestMethod -> URI
           -> [(SBS, SBS)] -> [(SBS, SBS)] -> [(SBS, SBS)]
           -> Either SBS w
           -> IO (Response LBS)
performReq verbose method path [] [] headers bodyE = do
    let req_0 = injectBody (mkRequest method path) bodyE
        req = foldl (\ req (k, v) -> replaceHeader (HdrCustom (cs k)) (cs v) req) req_0 headers

    when verbose $ do
        putStr "\n[=[REQUEST]============================================================\n"
        putStr $ show req <> "\n"
        putStr "\n-----------------------------------------------------------------------\n"
        LBS.putStr $ rqBody req <> "\n"
        putStr "\n------------------------------------------------------------[REQUEST]-]\n"

    responseE :: Either ConnError (Response LBS)
             <- simpleHTTP req

    case responseE of
        (Left err)      -> error $ show err
        (Right response) -> do
            when verbose $ do
                putStr "\n[-[RESPONSE]-----------------------------------------------------------\n"
                putStr $ show response <> "\n"
                putStr "\n----------------------------------------------------------------------\n"
                LBS.putStr $ rspBody response <> "\n"
                v <- case rspCode response of
                         code@(i, _, _) -> when (i /= 2) . putStr $
                             " *** failed with response code: " <> show code <> "\n"
                putStr "\n-----------------------------------------------------------[RESPONSE]-]\n"

            return response


performReqEmptyBody :: Bool -> RequestMethod -> URI -> [(SBS, SBS)] -> [(SBS, SBS)] -> [(SBS, SBS)] -> IO (Response LBS)
performReqEmptyBody verbose method path getps postps headers =
    performReq verbose method path getps postps headers (Left "" :: Either SBS JS.Value)


processResponse :: JS.FromJSON v => Bool -> Response LBS -> Maybe v
processResponse verbose_ response =
    case rspCode response of
          (2, _, _) -> JS.decode . cs . rspBody $ response
          code      -> Nothing


-- | Process response, force response code 2xx and remove 'Maybe' wrapper.
forceResponse :: JS.FromJSON v => Bool -> Response LBS -> v
forceResponse verbose_ response =
    maybe (error "giving up") id $ processResponse verbose_ response


-- | Process response, force response code 2xx and drop result entirely.
assertResponse :: Bool -> Response LBS -> ()
assertResponse verbose_ response =
    maybe (error "giving up") (const ()) $
      (processResponse verbose_ response :: Maybe JS.Value)


-- | Process response, force response code /= 2xx and drop result
-- entirely.  ('processResponse' above could also return an either
-- error that we can process here.)
assertError :: Bool -> Response LBS -> ()
assertError verbose_ response =
    maybe () (const $ error "giving up") $
      (processResponse verbose_ response :: Maybe JS.Value)
