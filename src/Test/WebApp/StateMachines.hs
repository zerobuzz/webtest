{-# LANGUAGE NoImplicitPrelude                        #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE ViewPatterns                             #-}
{-# LANGUAGE RecordWildCards                          #-}
{-# LANGUAGE NamedFieldPuns                           #-}

{- OPTIONS -fwarn-unused-imports #-}

-- | FIXME: (1) update documentation.  (2) this is about state
-- machines that mix both HTTP 'ScriptRq's / 'StoryItem's and a
-- webdriver equivalent; this means we need to change types 'Script'
-- and 'Story'.  for now, this module is only about HTTP.
module Test.WebApp.StateMachines
where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Data.Function
import Data.List as List
import Data.Map (Map)
import Data.Maybe
import Data.String.Conversions
import Prelude hiding ((++))
import Test.QuickCheck as QC
import Test.WebDriver
import Text.Printf
import Text.Show.Pretty

import qualified Data.Aeson as JS
import qualified Data.Map as Map
import qualified Network.HTTP as NH

import Test.WebApp.HTTP.Story
import Test.WebDriver.Missing
import Test.QuickCheck.Missing
import Test.WebApp.WebDriver hiding (State (..), StateId, Machine (..), Trace (..),
                                     propTrace, propTrace', predecessors, successors, ancestors, descendants,
                                     shortcuts, shortcuts', shortestTerminatingTrace, arbitraryTrace)
                               -- (all the above will eventually be merged into here.)



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
data State content =
    StateHTTP
      { stateStart        :: Bool
      , stateTerminal     :: Bool
      , stateTransitions  :: [StateId]
      , stateHTTPRequest  :: Script content -> Gen (ScriptRq content)
      , stateTest         :: Story content -> QC.Property
      }

instance Show (State content) where
    show (StateHTTP {..}) =
        printf "(StateHTTP %s %s %s)" (show stateStart) (show stateTerminal) (show stateTransitions)


-- | FIXME: this should be an application-specific enum type that is
-- passed to 'State', 'Machine', ... as a parameter.
type StateId = Int


stateHTTP :: [StateId]
          -> (Script content -> Gen (ScriptRq content))
          -> State content
stateHTTP trans step = StateHTTP True True trans step (const $ mkprop True)


-- | A machine is a mapping from 'StateId's to 'State's.  All other
-- information is contained in the states.
data Machine content = Machine { fromMachine :: Map StateId (State content) }
  deriving (Show)


-- | Collect all 'State's with 'StateId' of a machine, and create it.
-- Crashes if any 'StateId' occurs more than once.
mkMachine :: [(StateId, State content)] -> Machine content
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
--
-- FIXME: this should be called 'ProtoScript', and 'Script' should
-- move to this module and allow for both http and webdriver items.
data Trace content = Trace (Machine content) [StateId]
  deriving (Show)


arbitraryTrace :: forall content . Machine content -> Gen (Trace content)
arbitraryTrace machine = mkTrace <$> (mkStart >>= sized . f)
  where
    mkStart :: Gen StateId
    mkStart = case Map.keys . Map.filter stateStart . fromMachine $ machine of
                keys@(_:_) -> elements keys
                [] -> error $ "arbitraryTrace: machine without start state: " ++ ppShow machine

    mkTrace :: Maybe [StateId] -> Trace content
    mkTrace (Just ids) = Trace machine ids
    mkTrace Nothing = error $ "arbitraryTrace: machine without terminator state: " ++ ppShow machine

    f :: StateId -> Int -> Gen (Maybe [StateId])
    f last i = case (i, successors machine last) of
        (0, _)           -> terminate
        (_, [])          -> terminate
        (_, successors)  -> do
            x <- elements successors
            mxs <- f x (i-1)
            case mxs of
              Just xs -> return $ Just $ last:xs
              Nothing -> terminate
      where
        terminate = return $ shortestTerminatingTrace machine last


-- | Shrinking: This is currently just a call to 'shortcut'.  (Before
-- that, we should chop off the part of the script after the first
-- test failed, but this is done elsewhere.  FIXME: where?)
shrinkTrace :: Trace content -> [Trace content]
shrinkTrace = shortcuts


-- | A 'Trace' defines a 'Script'.  (Starting from the empty script,
-- follow the state ids and compute new requests from trace prefices.)
-- Since state transitions do not depend on the trace prefix, it is
-- useful to randomize it (and for that end wrap it into the 'Gen'
-- monad.)
arbitraryScriptFromTrace :: forall content . Trace content -> Gen (Script content)
arbitraryScriptFromTrace (Trace machine stateIds) = foldM generate (Script []) states
  where
    generate :: Script content -> State content -> Gen (Script content)
    generate script@(Script rqs) state = Script . (rqs ++) . (:[]) . number <$> rq
      where
        rq :: Gen (ScriptRq content)
        rq = case state of
                StateHTTP {..} -> stateHTTPRequest script

        number :: ScriptRq content -> ScriptRq content
        number rq = rq { srqSerial = length rqs }

    states :: [State content]
    states = catMaybes' $ map (`Map.lookup` (fromMachine machine)) stateIds

    catMaybes' [] = []
    catMaybes' (Just x:xs) = x : catMaybes' xs


-- ** Graph algorithms

-- | Keep the last state.  Find shortcuts in the earlier states that
-- maintain the validity of the trace (e.g., replace @2->3->1@ by
-- @2->1@ if @1@ is a direct successor of @2@ as well as one of @3@).
-- Output will only contain real shortcuts, not the unabbreviated
-- input trace.
--
-- FIXME: for now, this only cuts out proper cycles, not detours like
-- the example above.
shortcuts :: Trace content -> [Trace content]
shortcuts (Trace machine xs) = map (Trace machine . reverse) . shortcuts' machine . reverse $ xs


-- | Plumbing for 'shortcuts'.
shortcuts' :: Machine content -> [StateId] -> [[StateId]]
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
successors :: Machine content -> StateId -> [StateId]
successors machine id = maybe [] stateTransitions $ Map.lookup id $ fromMachine machine

-- | Produce all descentants of a state, ie. both direct successors
-- and their successors, recursively.  Each descentant is annotated
-- with the length of the path that it was reached on in the search
-- (not necessarily the length of the shortest path to it!).  Output
-- list is infinite iff there are cycles in the machine graph.
descendants :: Machine content -> StateId -> [(Int, StateId)]
descendants machine id = map (1,) direct ++ concat (map recursive direct)
  where
    direct = successors machine id
    recursive id' = map (first (+1)) $ descendants machine id'

-- | Like 'successors', but up the transition graph.  (In the current
-- implementation, this is a bit more expensive than down.)
predecessors :: Machine content -> StateId -> [StateId]
predecessors machine id = map fst . filter hit . Map.toList . fromMachine $ machine
  where
    hit (_, state) = id `elem` stateTransitions state

-- | Like 'descendants' to 'successors', but calls 'predecessors'
-- instead.
ancestors :: Machine content -> StateId -> [(Int, StateId)]
ancestors machine id = map (1,) direct ++ concat (map recursive direct)
  where
    direct = predecessors machine id
    recursive id' = map (first (+1)) $ ancestors machine id'

-- | FIXME: 'descendants' and 'shortestTerminatingTrace' should both
-- make use of more general graph algorithms.  Check king paper and
-- ghc libs once more!
shortestTerminatingTrace :: Machine content -> StateId -> Maybe [StateId]
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
    terminal sid = case Map.lookup sid (fromMachine machine) of Just s -> stateTerminal s

    sieve :: [[StateId]] -> [[StateId]]
    sieve = map snd
          . sortBy (flip compare `on` fst)
          . map (\ ids -> (length ids, ids))
