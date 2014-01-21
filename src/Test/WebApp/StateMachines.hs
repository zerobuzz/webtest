{-# LANGUAGE NoImplicitPrelude                        #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE ViewPatterns                             #-}

{- OPTIONS -fwarn-unused-imports #-}

module Test.WebApp.StateMachines
where

import Control.Applicative
import Control.Arrow
import Data.Function
import Data.List as List
import Data.Map (Map)
import Data.Maybe
import Data.String.Conversions
import Prelude hiding ((++))
import Test.QuickCheck as QC
import Test.WebDriver
import Text.Printf

import qualified Data.Aeson as JS
import qualified Data.Map as Map
import qualified Network.HTTP as NH

import Test.WebApp.HTTP.Story
import Test.WebDriver.Missing
import Test.QuickCheck.Missing
import Test.WebApp.WebDriver hiding (State (..), StateId, Machine (..), Trace (..),
                                     propTrace, propTrace', predecessors, successors, ancestors, descendants,
                                     shortcuts, shortcuts', shortestTerminatingTrace)
                               -- (all this will eventually be merged into here.)



-- * State machines

-- ** State

-- | FIXME: not sure this is the right way to do it.  if sTransitions
-- is [StateId], there is no way of making the set of successor states
-- depend on the current state.  can't change path if there is an
-- error, for instance.  if it is payload -> [StateId], there is no
-- way of doing traces statically - we can only make them up as we run
-- them.  there will also be no way of compiling them to python.
--
-- we could make it Either [StateId] [StateId], and pick Right in case
-- sTest returns truesy, and Left if it returns falsy.  so we would
-- feed one bit of the payload to the transition function.  (is that
-- better than an unknown large number of bits?  not really...  also,
-- no point: once the first test is falsy, execution will stop!)
--
-- for now, go with static transition maps, because otherwise it's
-- just a turing machine an we can't do anything interesting to it but
-- run it.
data State payload content =
    StateHTTP
      { sStart        :: Bool
      , sTerminal     :: Bool
      , sEnterHTTP    :: payload -> ScriptRq content
      , sUpdateHTTP   :: forall payload' . payload' ~ payload => payload -> Either (NH.Response LBS) content -> payload'
      , sTest         :: forall payload' . payload' ~ payload => payload' -> QC.Property
      , sTransitions  :: [StateId]
      }
  | StateWD
      { sStart        :: Bool
      , sTerminal     :: Bool
      , sEnterWD      :: payload -> WDSession -> WD (Maybe JS.Value)
      , sUpdateWD     :: forall payload' . payload' ~ payload => payload -> Maybe JS.Value -> payload'
      , sTest         :: forall payload' . payload' ~ payload => payload' -> QC.Property
      , sTransitions  :: [StateId]
      }


stateHTTP :: (payload -> ScriptRq content)
          -> (forall payload' . payload' ~ payload => payload -> Either (NH.Response LBS) content -> payload')
          -> [StateId]
          -> State payload content
stateHTTP enter update trans = StateHTTP True True enter update (const $ mkprop True) trans


stateWD :: (payload -> WDSession -> WD (Maybe JS.Value))
          -> (forall payload' . payload' ~ payload => payload -> Maybe JS.Value -> payload')
          -> [StateId]
          -> State payload content
stateWD enter update trans = StateWD True True enter update (const $ mkprop True) trans


instance Show (State payload content) where
    show (StateHTTP start term enter update test trans) =
        printf "(StateHTTP %s %s)" (show start) (show term)
    show (StateWD start term enter update test trans) =
        printf "(StateWD %s %s)" (show start) (show term)

type StateId = Int

-- | A machine is a mapping from 'StateId's to 'State's.  All other
-- information is contained in the states.
data Machine payload content = Machine { fromMachine :: Map StateId (State payload content) }
  deriving (Show)


-- | Collect all 'State's with 'StateId' of a machine, and create it.
-- Crashes if any 'StateId' occurs more than once.
mkMachine :: [(StateId, State payload content)] -> Machine payload content
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
data Trace payload content = Trace (Machine payload content) [StateId]


-- | Like 'Trace', but contains the updated payload of each state
-- after execution.
data Trace' payload content = Trace' (Machine payload content) [(StateId, payload)]


-- | Shrinking: This is currently just a call to 'shortcut'.  (Before
-- that, we should chop off the part of the script after the first
-- test failed, but this is done elsewhere.  FIXME: where?)
shrinkTrace :: Trace payload content -> [Trace payload content]
shrinkTrace = shortcuts


{-

arbitraryScript' :: Machine payload content -> Gen (Script content)
arbitraryScript' machine = Script . catMaybes . map (Map.lookup (fromMachine machine)) <$> (mkStart >>= sized . f)
  where
    mkStart :: Gen StateId
    mkStart = case Map.keys . Map.filter sStart . fromMachine $ machine of
                keys -> elements keys
                [] -> error $ "arbitraryTrace: machine without start state: " ++ ppShow machine

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

propTrace :: WDSession -> (a -> Trace payload content) -> a -> QC.Property
propTrace session fromTypedTrace typedTrace = propTrace' session (fromTypedTrace typedTrace)


-- | This property sais that a script satisfies all properties in all
-- intermediate states.
--
-- (This is sort of the replacement for the 'Story' type for http rest
-- api testing.  With selenium/webdriver, it is much less
-- straight-forward to represent the application state in a type
-- instantiating Show, Eq, ...)
propTrace' :: WDSession -> Trace payload content -> QC.Property
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

-}


-- ** Graph algorithms

-- | Keep the last state.  Find shortcuts in the earlier states that
-- maintain the validity of the trace (e.g., replace @2->3->1@ by
-- @2->1@ if @1@ is a direct successor of @2@ as well as one of @3@).
-- Output will only contain real shortcuts, not the unabbreviated
-- input trace.
--
-- FIXME: for now, this only cuts out proper cycles, not detours like
-- the example above.
shortcuts :: Trace payload content -> [Trace payload content]
shortcuts (Trace machine xs) = map (Trace machine . reverse) . shortcuts' machine . reverse $ xs


-- | Plumbing for 'shortcuts'.
shortcuts' :: Machine payload content -> [StateId] -> [[StateId]]
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
successors :: Machine payload content -> StateId -> [StateId]
successors machine id = maybe [] sTransitions $ Map.lookup id $ fromMachine machine

-- | Produce all descentants of a state, ie. both direct successors
-- and their successors, recursively.  Each descentant is annotated
-- with the length of the path that it was reached on in the search
-- (not necessarily the length of the shortest path to it!).  Output
-- list is infinite iff there are cycles in the machine graph.
descendants :: Machine payload content -> StateId -> [(Int, StateId)]
descendants machine id = map (1,) direct ++ concat (map recursive direct)
  where
    direct = successors machine id
    recursive id' = map (first (+1)) $ descendants machine id'

-- | Like 'successors', but up the transition graph.  (In the current
-- implementation, this is a bit more expensive than down.)
predecessors :: Machine payload content -> StateId -> [StateId]
predecessors machine id = map fst . filter hit . Map.toList . fromMachine $ machine
  where
    hit (_, state) = id `elem` sTransitions state

-- | Like 'descendants' to 'successors', but calls 'predecessors'
-- instead.
ancestors :: Machine payload content -> StateId -> [(Int, StateId)]
ancestors machine id = map (1,) direct ++ concat (map recursive direct)
  where
    direct = predecessors machine id
    recursive id' = map (first (+1)) $ ancestors machine id'

-- | FIXME: 'descendants' and 'shortestTerminatingTrace' should both
-- make use of more general graph algorithms.  Check king paper and
-- ghc libs once more!
shortestTerminatingTrace :: Machine payload content -> StateId -> Maybe [StateId]
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
