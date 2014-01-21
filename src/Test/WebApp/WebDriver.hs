{-# LANGUAGE NoImplicitPrelude                        #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE ViewPatterns                             #-}

{-# OPTIONS -fwarn-unused-imports #-}

{-| This module uses Adam Curtis' webdriver binding.  You need to
download and run server hub and node processes (see
http://docs.seleniumhq.org/download/ and wiki page linked from there).

Instead of using a headless browser, on debian you can simply run this
(as root):

> apt-get install xvfb
> nohup Xvfb :100 -ac > /dev/null 2> /dev/null &
> DISPLAY=:100 pybot unit

TODO: checkout packages free, sunroof!

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
import qualified Data.Map as Map
import qualified Data.Text as ST

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
hijackBrowserConsole :: WebDriver wd => wd ()
hijackBrowserConsole =
    evalJS' []
        [ "if (typeof " <> jsscopeGet "__console__" <> " === 'undefined') {"
        , "    " <> jsscopeSet "__console__" "[]"
        , "    var log_ = console.log;"
        , "    console.log = function(...args) {"
        , "        " <> jsscopeGet "__console__" <> ".push(args);"
        , "        log_(args);"
        , "    };"
        , "}"
        ]


-- | Download copy of browser logs.  This only works from the moment
-- you call 'hijackBrowserConsole'.
getBrowserConsole :: WD [[JS.Value]]
getBrowserConsole = evalJS [] ["return " <> jsscopeGet "__console__"]


-- | Dump browser logs to stdout (in 'WD').  Only works from the
-- moment you call 'hijackBrowserConsole'.
printBrowserConsole :: WD ()
printBrowserConsole = getBrowserConsole >>= liftIO . putStrLn . ppShow


-- | Dump browser logs to stdout (in 'IO').  Only works from the
-- moment you call 'hijackBrowserConsole'.
printBrowserConsoleIO :: WDSession -> IO ()
printBrowserConsoleIO session = runWD session getBrowserConsole >>= putStrLn . ppShow



-- * State machines (this will be obsoleted eventually by module Test.WebApp.StateMachines

-- | Test State with quickcheck property and transition actions.
--
-- There are several options as to where to put the properties to be
-- tested (the state invariants).  Some tests can be done from server
-- debug logs (like like 4xx or 5xx responses).  Some can be done
-- globally over a run trace of the state machine.
--
-- The latter would fit more easily into the quickcheck toolbox.  It
-- works well with testing rest protocols, but this is not really what
-- we want for UI testing because we cannot store the entire state of
-- the browser that we want to inspect.
--
-- The flags 'sStart' and 'sTerminal' state whether traces are allowed
-- to start / end with this state.
--
-- 'sTest' is an optional quickcheck property in the 'WD' monad that
-- is tested in the trace run.
--
-- 'sTransitions' is a set of target state Ids with actions that enter
-- that state.  The state machine is non-deterministic: when
-- generating a run trace, any one of the transitions is selected at
-- random.
data State = State
      { sStart        :: Bool
      , sTerminal     :: Bool
      , sTest         :: Maybe (WD QC.Property)
      , sTransitions  :: [(StateId, WD ())]
      }

instance Show State where
    show (State start term test trans) =
        printf "(State %s %s %s)" (show start) (show term) (show $ map fst trans)

-- | For trace shrinking, 'State' must have an Id type in 'Ord', 'Eq', etc.
type StateId = Int

-- | A machine is a mapping from 'StateId's to 'State's.  All other
-- information is contained in the states.
data Machine = Machine { fromMachine :: Map StateId State }
  deriving (Show)


-- | Collect all 'State's with 'StateId' of a machine, and create it.
-- Crashes if any 'StateId' occurs more than once.
mkMachine :: [(StateId, State)] -> Machine
mkMachine states = if null dupeIds
                       then Machine $ Map.fromList states
                       else error $ "mkMachine: duplicate StateIds: " ++ show dupeIds
  where
    ids = map fst states
    dupeIds = ids \\ nub ids


-- | 'Machine' is an abstract specification of all sessions, or of
-- what could possibly ever happen.  The thing we want to implement
-- 'arbitrary' and 'shrink' on is one particular session at a time,
-- with all state transition events chronological order.  We call this
-- a 'Trace'.
data Trace = Trace Machine [StateId]

instance Show Trace where
    show (Trace machine ids) = "Trace " <> show machine <> " " <> show ids


-- | 'Trace's are generated from 'Machine's.
arbitraryTrace :: Machine -> Gen Trace
arbitraryTrace machine = Trace machine <$> (mkStart >>= sized . f)
  where
    mkStart :: Gen StateId
    mkStart = case Map.keys $ Map.filter sStart $ fromMachine machine of
                [] -> error $ "arbitraryTrace: machine without start state: " ++ ppShow machine
                keys -> elements keys

    terminate :: StateId -> Gen [StateId]
    terminate sid = case shortestTerminatingTrace machine sid of
        Just xs -> return xs
        Nothing -> fail $ "arbitraryTrace: failed to find terminating path for machine " ++ show machine

    f :: StateId -> Int -> Gen [StateId]
    f last i = case (i, successors machine last) of
        (0, _)           -> terminate last
        (_, [])          -> terminate last
        (_, successors)  -> do
            x <- elements successors
            ((last:) <$> f x (i-1))  -- FIXME: <|> terminate last


-- | Shrinking: This is currently just a call to 'shortcut'.  (Before
-- that, we should chop off the part of the script after the first
-- test failed, but this is done elsewhere.  FIXME: where?)
shrinkTrace :: Trace -> [Trace]
shrinkTrace = shortcuts


propTrace :: WDSession -> (a -> Trace) -> a -> QC.Property
propTrace session fromTypedTrace typedTrace = propTrace' session (fromTypedTrace typedTrace)


-- | This property sais that a script satisfies all properties in all
-- intermediate states.
--
-- (This is sort of the replacement for the 'Story' type for http rest
-- api testing.  With selenium/webdriver, it is much less
-- straight-forward to represent the application state in a type
-- instantiating Show, Eq, ...)
propTrace' :: WDSession -> Trace -> QC.Property
propTrace' session trace@(Trace machine []) = mkprop True  -- empty traces are boring
propTrace' session trace@(Trace machine xs) =
    morallyDubiousIOProperty $ do
      -- debugM "" $ "propTrace: running " ++ ppShow trace
      f xs
  where
    f :: [StateId] -> IO QC.Property
    f (idThis : idsRest) = do
        debugM "" $ "propTrace': " ++ show idThis

        let stateThis :: State
            Just stateThis = Map.lookup idThis $ fromMachine machine

        let runTest :: IO QC.Property
            runTest = runWD session $ maybe successWD id $ sTest stateThis

        case (sTerminal stateThis, idsRest) of
          (True, [])
              -> runTest
          (False, [])
              -> fail $ "propTrace': Trace ends in non-terminal state: " ++ ppShow (idThis, stateThis, trace)
          (_, (idNext:_))
              -> let transition :: WD ()
                     transition = case lookup idNext $ sTransitions stateThis of
                        Just t -> t
                        Nothing -> error $ "propTrace': internal error: " ++ show (idNext, stateThis)

                     step :: IO QC.Property
                     step = runTest `finally` runWD session transition

                     recurse :: IO QC.Property
                     recurse = f idsRest

                 in (.&&.) <$> step <*> recurse


-- FIXME: check out Test.QuickCheck.Monadic


-- | Keep the last state.  Find shortcuts in the earlier states that
-- maintain the validity of the script (e.g., replace @2->3->1@ by
-- @2->1@ if @1@ is a direct successor of @2@ as well as one of @3@).
-- Output will only contain real shortcuts, not the unabbreviated
-- input 'Trace'.
--
-- FIXME: for now, this only cuts out proper cycles, not detours like
-- the example in the documentation of 'shortcuts'.
shortcuts :: Trace -> [Trace]
shortcuts (Trace machine xs) = map (Trace machine . reverse) . shortcuts' machine . reverse $ xs


-- | Plumbing for 'shortcuts'.
shortcuts' :: Machine -> [StateId] -> [[StateId]]
shortcuts' machine [] = []
shortcuts' machine [only] = []
shortcuts' machine (last:before@(second:before')) = (last:) <$> (direct before' ++ recursive)
  where
    direct :: [StateId] -> [[StateId]]
    direct [] = []
    direct candidate@(x:xs) = (if x == second then (candidate:) else id) $ direct xs

    recursive :: [[StateId]]
    recursive = shortcuts' machine before


-- | Produce all possible direct successors (connected via a single
-- transition) of a state.
successors :: Machine -> StateId -> [StateId]
successors machine id = maybe [] (map fst . sTransitions) $ Map.lookup id $ fromMachine machine

-- | Produce all descentants of a state, ie. both direct successors
-- and their successors, recursively.  Each descentant is annotated
-- with the length of the path that it was reached on in the search
-- (not necessarily the length of the shortest path to it!).  Output
-- list is usually infinite.
descendants :: Machine -> StateId -> [(Int, StateId)]
descendants machine id = map (1,) direct ++ concat (map recursive direct)
  where
    direct = successors machine id
    recursive id' = map (first (+1)) $ descendants machine id'

-- | Like 'successors', but up the transition graph.  (In the current
-- implementation, this is a bit more expensive than down.)
predecessors :: Machine -> StateId -> [StateId]
predecessors machine id = map fst . filter hit . Map.toList . fromMachine $ machine
  where
    hit (_, state) = id `elem` map fst (sTransitions state)

-- | Like 'descendants' to 'successors', but calls 'predecessors'
-- instead.
ancestors :: Machine -> StateId -> [(Int, StateId)]
ancestors machine id = map (1,) direct ++ concat (map recursive direct)
  where
    direct = predecessors machine id
    recursive id' = map (first (+1)) $ ancestors machine id'

-- | FIXME: 'descendants' and 'shortestTerminatingTrace' should both
-- make use of more general graph algorithms.  Check king paper and
-- ghc libs once more!
shortestTerminatingTrace :: Machine -> StateId -> Maybe [StateId]
shortestTerminatingTrace machine start = listToMaybe $ step [] start
  where
    step :: [StateId] -> StateId -> [[StateId]]
    step visited now
        | now `elem` visited  = []        -- (give up when hitting a cycle.)
        | terminal now        = [[now]]   -- (end when hitting a terminal state.)
        | True                = let recurse :: [[StateId]]
                                    recurse = concatMap (step (now:visited)) (successors machine now)
                                in sieve ((now:) <$> recurse)

    terminal :: StateId -> Bool
    terminal sid = case Map.lookup sid (fromMachine machine) of Just s -> sTerminal s

    sieve :: [[StateId]] -> [[StateId]]
    sieve = map snd
          . sortBy (flip compare `on` fst)
          . map (\ ids -> (length ids, ids))
