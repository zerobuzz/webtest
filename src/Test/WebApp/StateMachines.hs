{-# LANGUAGE NoImplicitPrelude                        #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
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
import Data.Typeable
import GHC.Generics
import Data.List as List
import Data.Map (Map)
import Data.Maybe
import Data.String.Conversions
import Network.URI
import Prelude hiding ((++))
import Test.QuickCheck as QC
import Test.WebDriver
import Text.Printf
import Text.Show.Pretty

import qualified Data.Aeson as JS
import qualified Data.Map as Map
import qualified Network.HTTP as NH

import Test.WebApp.HTTP.Story hiding (Script (..), Story (..))
import Test.WebDriver.Missing
import Test.QuickCheck.Missing
import Test.WebApp.WebDriver hiding (State (..), StateId, Machine (..), Trace (..),
                                     propTrace, propTrace', predecessors, successors, ancestors, descendants,
                                     shortcuts, shortcuts', shortestTerminatingTrace, arbitraryTrace)
                               -- (all the above will eventually be merged into here.)



-- * State machines

data SM sid content = SM { fromSM :: Map sid (State sid content) }
  deriving (Show)

data State sid content =
      State
        { stateId           :: sid
        , stateStart        :: Bool
        , stateTerminal     :: Bool
        , stateTransitions  :: [(Script sid content -> Gen (ScriptItem content), [sid])]
        }
  deriving (Typeable)

instance (Show sid) => Show (State sid content) where show s = "(State " <> show (stateId s) <> ")"

data Script sid content =
      Script
        { scriptSM    :: SM sid content
        , scriptItems :: [(sid, ScriptItem content)]
        }
  deriving (Typeable)

data Trace sid content =
      Trace
        { traceSM     :: SM sid content
        , traceItems  :: [(sid, TraceItem content)]
        }
  deriving (Typeable)

data ScriptItem content =
      ScriptItemHTTP
        { siSerial       :: Ix
        , siMethod       :: NH.RequestMethod
        , siBody         :: Either SBS content
        , siGetParams    :: [(SBS, SBS)]
        , siPostParams   :: [(SBS, SBS)]
        , siHeaders      :: [(SBS, SBS)]
        , siPath         :: Either URI IxRef
        }
  deriving (Show, Eq, Typeable, Generic)

data TraceItem content =
      TraceItemHTTP
        { tiReq :: ScriptRq content
        , tiRsp :: Maybe (NH.Response LBS)
        }
  deriving (Show, Typeable, Generic)


mkMachine :: (Eq sid, Enum sid, Bounded sid, Ord sid, Show sid)
          => [State sid content] -> SM sid content
mkMachine states = if null dupeIds
                       then SM $ Map.fromList (zip ids states)
                       else error $ "mkMachine: duplicate StateIds: " ++ show dupeIds
  where
    ids = map stateId states
    dupeIds = ids \\ nub ids



{-

arbitraryScript :: forall sid content . SM sid content -> Gen (Script sid content)
arbitraryScript machine = mkScript <$> (mkStart >>= sized . f)
  where
    mkStart :: Gen sid
    mkStart = case Map.keys . Map.filter stateStart . fromSM $ machine of
                keys@(_:_) -> elements keys
                [] -> error $ "arbitraryTrace: machine without start state: " ++ ppShow machine

    mkScript :: Maybe [(sid, ScriptItem content)] -> Script sid content
    mkScript (Just ids) = Script machine ids
    mkScript Nothing = error $ "arbitraryTrace: machine without terminator state: " ++ ppShow machine

    f :: sid -> Int -> Gen (Maybe [(sid, ScriptItem content)])
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
shortcuts :: Script sid content -> [Script sid content]
shortcuts (Script machine xs) = error "" -- map (Script machine . reverse) . shortcuts' machine . reverse $ xs


-- | Plumbing for 'shortcuts'.
shortcuts' :: forall sid . (Eq sid) => [sid] -> [[sid]]
shortcuts' [] = []
shortcuts' [only] = []
shortcuts' (last:before@(second:before')) = (last:) <$> (direct before' ++ recursive)
  where
    direct :: [sid] -> [[sid]]
    direct [] = []
    direct candidate@(x:xs) = (if x == second then (candidate:) else id) $ direct xs

    recursive :: [[sid]]
    recursive = shortcuts' before


-- | Produce all possible direct successors (connected via a single
-- transition) of a state.
successors :: forall sid content . (Ord sid)
           => SM sid content -> sid -> [sid]
successors machine sid = maybe [] (nub . join . map snd . stateTransitions) $ Map.lookup sid $ fromSM machine

-- | Produce all descentants of a state, ie. both direct successors
-- and their successors, recursively.  Each descentant is annotated
-- with the length of the path that it was reached on in the search
-- (not necessarily the length of the shortest path to it!).  Output
-- list is infinite iff there are cycles in the machine graph.
descendants :: forall sid content . (Ord sid)
            => SM sid content -> sid -> [(Int, sid)]
descendants machine id = map (1,) direct ++ concat (map recursive direct)
  where
    direct = successors machine id
    recursive id' = map (first (+1)) $ descendants machine id'

-- | Like 'successors', but up the transition graph.  (In the current
-- implementation, this is a bit more expensive than down.)
predecessors :: forall sid content . (Eq sid) => SM sid content -> sid -> [sid]
predecessors machine id = map fst . filter hit . Map.toList . fromSM $ machine
  where
    hit (_, state) = id `elem` (nub . join . map snd . stateTransitions) state

-- | Like 'descendants' to 'successors', but calls 'predecessors'
-- instead.
ancestors :: forall sid content . (Eq sid) => SM sid content -> sid -> [(Int, sid)]
ancestors machine id = map (1,) direct ++ concat (map recursive direct)
  where
    direct = predecessors machine id
    recursive id' = map (first (+1)) $ ancestors machine id'

-- | FIXME: 'descendants' and 'shortestTerminatingTrace' should both
-- make use of more general graph algorithms.  Check king paper and
-- ghc libs once more!
shortestTerminatingTrace :: forall sid content . (Eq sid, Ord sid) => SM sid content -> sid -> Maybe [sid]
shortestTerminatingTrace machine start = listToMaybe $ step [] start
  where
    step :: [sid] -> sid -> [[sid]]
    step visited now
        | now `elem` visited  = []        -- (give up when hitting a cycle.)
        | terminal now        = [[now]]   -- (end when hitting a terminal state.)
        | True                = let recurse :: [[sid]]
                                    recurse = concatMap (step (now:visited)) (successors machine now)
                                in sieve ((now:) <$> recurse)

    terminal :: sid -> Bool
    terminal sid = case Map.lookup sid (fromSM machine) of Just s -> stateTerminal s

    sieve :: [[sid]] -> [[sid]]
    sieve = map snd
          . sortBy (flip compare `on` fst)
          . map (\ ids -> (length ids, ids))
