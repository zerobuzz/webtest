{-# LANGUAGE NoImplicitPrelude                        #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE ViewPatterns                             #-}

{- OPTIONS -fwarn-unused-imports #-}

{-| This module extends Adam Curtis' webdriver binding.  You need to
download and run server hub and node processes (see
http://docs.seleniumhq.org/download/ and wiki page linked from there).

Instead of using a headless browser, on debian you can simply run this
(as root):

> apt-get install xvfb
> nohup Xvfb :100 -ac > /dev/null 2> /dev/null &
> DISPLAY=:100 pybot unit

TODO:

 - some things in this module should go to "Test.WebDriver.Missing"
   and eventually to the webdriver package.  not sure what the idea of
   this module will be when those things are gone.

 - checkout package sunroof!

-}
module Test.WebApp.WebDriver
where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import Data.List as List
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.String.Conversions
import Prelude hiding ((++))
import System.Log.Logger
import Test.QuickCheck as QC
import Test.QuickCheck.Property as QC
import Test.WebDriver
import Test.WebDriver.Classes
import Text.Printf
import Text.Show.Pretty

import qualified Data.Aeson as JS
import qualified Data.Aeson.Encode.Pretty as JS
import qualified Data.Map as Map
import qualified Data.Text as ST
import qualified Data.ByteString.Lazy as LBS

import Test.QuickCheck.Missing



-- * Little things

-- | Frontend for 'runWD'.  All exceptions are caught; the exception
-- handler fetches the browser console output and writes both
-- exception and console output to stdout, then crashes.  Before
-- calling this, call 'hijackBrowserConsole' (in a call to 'runWD').
--
-- (MISSING: take a screenshot, store it to a temp file, and output
-- the file name together with the error log and the exception.)
runWD' :: WDSession -> WD a -> IO a
runWD' session action = do
    result <- catch (Right <$> runWD session action)
                    (\ (e :: SomeException) -> runWD session getBrowserConsole >>= return . Left . (e,))
    case result of
        Right a -> return a
        (Left (e, console)) -> putStrLn (ppShow console) >> putStrLn (ppShow e) >> error "runWD' failed!"

sleepIO :: Double -> IO ()
sleepIO seconds = Control.Concurrent.threadDelay (round (seconds * 1e6))

sleepWD :: Double -> WD ()
sleepWD = liftIO . sleepIO

successIO :: IO QC.Property
successIO = return $ mkprop True

successWD :: WD QC.Property
successWD = return $ mkprop True

got404 :: WD Bool
got404 = error "got404: not sure how to implement this..."

got500 :: WD Bool
got500 = error "got500: not sure how to implement this..."



-- * The eval* api

-- | Frontend for 'executeJS'.  First argument contains a list of
-- mods, second a list of args.  Both have nicks assigned to them that
-- can be used from inside javascript.  Example:
--
-- > v :: JS.Value <- evalJS [("vector", JSArg [0 :: Int, 1, 2, 3, 4]), ("index", JSArg (2 :: Int))]
-- >                         ["return [JSON.stringify(vector, index)];"]
--
-- FIXME: it would be neat if argument could also be a javascript
-- expression that will be evaluated once, and assigned to a nick.
-- Could be done with @Either String JSArg@ instead of @JSArg@.
evalJS :: (WebDriver wd, JS.FromJSON a) => [(ST, JSArg)] -> [ST] -> wd a
evalJS = evalJS_ executeJS False

-- | call 'evalJS' and ignore the return value.
evalJS' :: (WebDriver wd) => [(ST, JSArg)] -> [ST] -> wd ()
evalJS' args code = do (x :: JS.Value) <- evalJS args code; return ()

-- | Asyncrhonous variant of 'evalJS' that calls 'asyncJS' instead of
-- 'executeJS'.
evalAsyncJS :: (WebDriver wd, JS.FromJSON a) => [(ST, JSArg)] -> [ST] -> wd (Maybe a)
evalAsyncJS = evalJS_ asyncJS True

-- | call 'evalAsyncJS' and ignore the callback's return value.
-- returns True if callback was called, and False if a timeout was
-- hit.  (this is interesting if you want to know that the callback
-- has been called, but not what it has been called with.  if you
-- don't care about the latter, calling 'evalJS'' may be better
-- because it returns immediately.)
--
-- FIXME: i think timeouts trigger exceptions, not 'Nothing' return
-- values.  test this / check github ticket.
evalAsyncJS' :: (WebDriver wd) => [(ST, JSArg)] -> [ST] -> wd Bool
evalAsyncJS' args code = do (x :: Maybe JS.Value) <- evalAsyncJS args code; return (isJust x)

-- | Shared code of 'evalJS' and 'evalAsyncJS'.
evalJS_ :: ([JSArg] -> ST -> a) -> Bool -> [(ST, JSArg)] -> [ST] -> a
evalJS_ callff callback args body = callff (map snd args) body'
  where
    body' = ST.intercalate "\n" $ argsCode ++ body

    argsCode :: [ST]
    argsCode = zipWith f (map fst args ++ argscb) [0..]
      where
        argscb :: [ST]
        argscb = if callback then ["callback"] else []

        f :: ST -> Int -> ST
        f nick i = "var " <> nick <> " = arguments[" <> cs (show i) <> "];"


-- | Open module and add it into the scope.
evalRegisterModule :: (WebDriver wd) => ST -> ST -> wd ()
evalRegisterModule nick path = evalJS' [("path", JSArg path)] [jsscopeSet nick ("require(path)")]


-- | Trigger angular service factory and store created service into
-- the scope (under its own name).  Accepts a list of angular modules
-- required for constructing the injector ("ng" is implicit).
evalRegisterService :: (WebDriver wd) => ST -> [ST] -> ST -> wd ()
evalRegisterService serviceNick angularModules serviceName =
    evalJS' [("mods", JSArg ("ng" : angularModules)), ("service", JSArg serviceName)]
        [ "var i = angular.injector(mods);"
        , jsscopeSet serviceNick $ "i.get(service)"
        ]


-- | List alles names defined in scope.
evalScopeShow :: (WebDriver wd) => wd [ST]
evalScopeShow = evalJS [] ["return Object.keys(" <> jsscope <> ");"]

-- | Get name from global javascript scope into local namespace.
evalScopeGet :: (WebDriver wd, JS.FromJSON a) => ST -> wd a
evalScopeGet k = evalJS [] ["return " <> k <> ";"]

-- | Store expressoin in javascript scope.
evalScopeSet :: (WebDriver wd) => ST -> ST -> wd ()
evalScopeSet k expr = evalJS' [] [jsscopeSet k expr]

-- | Delete name from javascript scope.
evalScopeDelete :: (WebDriver wd) => ST -> wd ()
evalScopeDelete k = evalJS' [] [jsscopeDelete k]

-- | Clear scope.  If scope is initialized, empty it.  If it is not,
-- initialize it.
evalScopeClear :: (WebDriver wd) => wd ()
evalScopeClear = evalJS' [] [jsscopeClear]


-- | JS code: Get name from global javascript scope.
jsscopeGet :: ST -> ST
jsscopeGet k = mconcat [jsscope, ".", k]

-- | JS code: Update name in global javascript scope.
jsscopeSet :: ST -> ST -> ST
jsscopeSet k v = mconcat [jsscopeGet k, " = ", v, ";"]

-- | JS code: Delete name in global javascript scope.
jsscopeDelete :: ST -> ST
jsscopeDelete k = mconcat ["delete ", jsscopeGet k, ";"]

-- | JS code: Empty scope; if scope was not initialized, initialize it.
jsscopeClear :: ST
jsscopeClear = mconcat ["delete ", jsscope]


-- | JS code: Name of a javascript object that we can (we hope) use to
-- store names that we want to reuse between calls to 'evalJS'.
jsscope :: ST
jsscope = mconcat ["(() => { if (typeof ", location, " === 'undefined') { ", location, " = {}; }; return ", location, "; })()"]
  where
    location = "window.__webdriver_jsscope__"



-- * Waiting for things

-- | Send a js call that runs a piece of synchronous test code every
-- 'tickms' miliseconds.  If the test code returns true for
-- 'stableticks' consecutive ticks, 'waitForCondition' returns @True@;
-- otherwise (if it returns false or throws an exception), it returns
-- @False@.  (See also 'setScriptTimeout'.)
waitForCondition :: WebDriver wd => Int -> Int -> [(ST, JSArg)] -> [ST] -> wd Bool
waitForCondition tickms stableticks args code | stableticks >= 1 = evalAsyncJS' args' code'
  where
    verbose = False  -- (just for debugging)

    args' = [("__tickms__", JSArg tickms), ("__stableticks__", JSArg stableticks)] ++ args
    code' =
        "function __check_condition__() {" :
        ["    console.log('__check_condition__');" | verbose ] ++
        "" :
        "    try {" :
        map ("        " <>) code ++
        "    } catch (e) {" :
        ["        console.log('__check_condition__.catch', e);" | verbose ] ++
        "        return false;" :
        "    }" :
        "}" :
        "" :
        "function __loop_wait__() {" :
        ["    console.log('__loop_wait__');" | verbose ] ++
        "" :
        "    if (__check_condition__()) {" :
        "        setTimeout(() => __loop_stable__(__stableticks__), __tickms__);" :
        "    } else {" :
        "        setTimeout(__loop_wait__, __tickms__);" :
        "    }" :
        "}" :
        "" :
        "function __loop_stable__(ticks) {" :
        ["    console.log('__loop_stable__', ticks);" | verbose ] ++
        "" :
        "    if (ticks <= 1) {" :
        "        callback();" :
        "    } else {" :
        "        if (__check_condition__()) {" :
        "            setTimeout(() => __loop_stable__(ticks - 1), __tickms__);" :
        "        } else {" :
        "            setTimeout(__loop_wait__, __tickms__);" :
        "        }" :
        "    }" :
        "}" :
        "" :
        "    __loop_wait__();" :
        []

waitForConditionNgScope :: WebDriver wd => Int -> Int -> (ST, Element) -> [(ST, JSArg)] -> [ST] -> wd Bool
waitForConditionNgScope tickms stableticks (scopeNick, element) args code = waitForCondition tickms stableticks args' code'
  where
    args' = ("element", JSArg element) : args
    code' = ("var " <> scopeNick <> " = angular.element(element).scope();") : code



-- * Browser state inspection

newtype ConsoleLog = ConsoleLog { fromConsoleLog :: JS.Value }
  deriving (Eq, Show)

-- | Overload console.log method and push all logged data to
-- 'jsscope'.  See also 'getBrowserConsole'.  You should try to not
-- call this more than once because that wouldn't be very elegant, but
-- 'hijackBrowserConsole' handles multiple calls in one session
-- correctly.
--
-- (Is there a way to turn on the js debugger via selenium?  the
-- workaround would be to insert sleep in your test, then turn it on
-- manually.)
--
-- Also of possible interest:
--
-- > screenshot >>= liftIO . LBS.writeFile "/tmp/x.png"
-- > getLogTypes >>= mapM getLogs >>= liftIO . mapM (mapM (putStrLn . show))
-- > serverStatus >>= liftIO . print
--
-- NOTE: Console download is a bit fragile because e.g. 'openPage'
-- destroys the javascript heap, and thus also the local copy of the
-- console.
hijackBrowserConsole :: WebDriver wd => wd ()
hijackBrowserConsole =
    evalJS' []
        [ "if (typeof window.__console__ === 'undefined') {"
        , "    var log_ = console.log;"
        , "    __console__ = [];"
        , "    console.log = function() {"
        , "        window.__console__.push(arguments);"
        , "        log_.apply(console, arguments);"
        , "    };"
        , "}"
        ]


-- | Download copy of browser logs.  This only works from the moment
-- you call 'hijackBrowserConsole'.
getBrowserConsole :: WebDriver wd => wd ConsoleLog
getBrowserConsole = ConsoleLog <$> evalJS [] ["return window.__console__"]


-- | Dump browser logs to stdout (in 'WD').  Only works from the
-- moment you call 'hijackBrowserConsole'.
printBrowserConsole :: (MonadIO wd, WebDriver wd) => wd ()
printBrowserConsole = getBrowserConsole >>= liftIO . LBS.putStr . (<> "\n") . JS.encodePretty . fromConsoleLog
