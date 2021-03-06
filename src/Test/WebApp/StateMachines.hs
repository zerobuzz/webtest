{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE NamedFieldPuns                           #-}
{-# LANGUAGE NoImplicitPrelude                        #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE PackageImports                           #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE RecordWildCards                          #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE ViewPatterns                             #-}

{-# OPTIONS -fwarn-unused-imports #-}

-- | State machines are a formalism for writing 'Arbitrary' instances
-- of the 'Script' data types that behave as expected by a UI or REST
-- API.  This is hoped to be easier than writing instances from
-- scratch, but it has other benefits, like a new state-based
-- graphical representation of 'Script's and a free shrinking
-- heuristic that finds short cuts through the transition graph.
module Test.WebApp.StateMachines
where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.List as List
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.String.Conversions
import Data.Typeable
import Language.Dot as D
import Network.HTTP
import Network.URI
import Prelude hiding ((++))
import Test.QuickCheck as QC
import Text.Show.Pretty

import qualified Data.Aeson as JS
import qualified Data.Map as Map

import Test.WebApp.Script



-- * data type

-- | The state machine type.
data SM sid content = SM { fromSM :: Map sid (State sid content) }
  deriving (Show)

data State sid content =
      State
        { stateId           :: sid
        , stateStart        :: Bool
        , stateTerminal     :: Bool
        , stateTransitions  :: [Script sid content -> Gen (ScriptItem sid content, sid)]
        , stateTraceInv     :: Maybe (TraceItem sid content -> Trace sid content -> Maybe Bool)
        }
  deriving (Typeable)

instance (Show sid) => Show (State sid content) where
    show s = "<State " <> show (stateId s) <> ">"


-- | A constructor for state machines that enforces unique 'stateId's.
mkMachine :: (Eq sid, Enum sid, Bounded sid, Ord sid, Show sid)
          => [State sid content] -> SM sid content
mkMachine states = if null dupeIds
                       then SM $ Map.fromList (zip ids states)
                       else error $ "mkMachine: duplicate StateIds: " ++ show dupeIds
  where
    ids = map stateId states
    dupeIds = ids \\ nub ids


-- | Lookup a 'State' in an 'SM'.
getState :: (Ord sid) => SM sid content -> sid -> Maybe (State sid content)
getState (SM m) sid = Map.lookup sid m



-- * arbitrary scripts

-- | arbitrary scripts from a given state machine.
scriptFromSM :: forall sid content . (Ord sid, Show sid, Show content)
             => SM sid content -> Gen (Script sid content)
scriptFromSM machine = mkStart >>= \ state -> sized $ \ size -> mkItem (size + 1) (Script []) state
  where
    mkStart :: Gen (State sid content)
    mkStart = case Map.elems . Map.filter stateStart . fromSM $ machine of
                states@(_:_) -> elements states
                [] -> error $ "arbitraryScript: machine without start state: " ++ ppShow machine

    mkItem :: Int -> Script sid content -> State sid content -> Gen (Script sid content)
    mkItem size script lastState =
        case (stateTransitions lastState, stateTerminal lastState, size > 0) of
            ([], True,  _)     -> return script
            (_,  True,  False) -> return script
            ([], False, _)     -> error $ "arbitraryScript: no successors in non-terminal: " ++ show lastState
            (_,  False, False) -> continue 11 4
            (_,  _,     _)     -> continue 1 1
      where
        -- if w1 == w2 == 1, just generate one arbitrary successor and
        -- one terminating script.  otherwise, fan out many arbitrary
        -- successor states and script suffices, heuristically looking
        -- for the shortest script to any terminating state.
        continue :: Int -> Int -> Gen (Script sid content)
        continue w1 w2 | w1 > 0 && w2 > 0 = do
            transitions :: [(ScriptItem sid content, sid)]
                <- preferTerminals <$> (vectorOf w1 (elements (stateTransitions lastState)) >>= mapM ($ script))

            scripts :: [Script sid content]
                <- join <$>
                   mapM (\ (thisItem, thisSid) -> do
                            let thisItem' :: ScriptItem sid content
                                thisItem' = thisItem { siSerial     = Ix $ length (scriptItems script)
                                                     , siFromState  = Just $ stateId lastState
                                                     , siThisState  = Just $ stateId thisState
                                                     }
                                Just thisState = Map.lookup thisSid $ fromSM machine

                            vectorOf w2 (mkItem (size - 1) (script <> Script [thisItem']) thisState)
                        ) transitions

            return $ shortestScript scripts

        preferTerminals :: [(ScriptItem sid content, sid)] -> [(ScriptItem sid content, sid)]
        preferTerminals = sortBy (\ a b -> f (g a) (g b))
            where
                g :: (ScriptItem sid content, a) -> Maybe (State sid content)
                g (a, sid) = siThisState a >>= getState machine

                f :: Maybe (State sid content) -> Maybe (State sid content) -> Ordering
                f (Just (stateTerminal -> True)) (Just (stateTerminal -> False)) = EQ
                f (Just (stateTerminal -> True)) (Just _) = GT
                f _ _ = EQ

        shortestScript :: [Script sid content] -> Script sid content
        shortestScript = f Nothing
            where
                f _         (x:_) | length (scriptItems x) == 1  = x  -- no need to keep looking!
                f _         (x:_) | length (scriptItems x) < 1   = error "arbitraryScript: internal error."
                f Nothing   (x:xs)                               = f (Just x)                  xs
                f (Just x') (x:xs)                               = f (Just (pickShorter x x')) xs
                f (Just x') []                                   = x'
                f acc rest = error $ "arbitraryScript.shortestScript: unmatched pattern: " ++ show (acc, rest)

                pickShorter x x' = if length (scriptItems x) < length (scriptItems x')
                                     then x
                                     else x'


prop_scriptFromSM_serials :: (Ord sid, Show sid, Show property) => SM sid property -> QC.Property
prop_scriptFromSM_serials sm = forAll (scriptFromSM sm) $ and . zipWith (==) [(Ix 0)..] . map siSerial . scriptItems



-- * shrinking scripts

-- | Cut out cycles in the state machine's transition graph: If some
-- state occurs more than once in the 'Script', drop all states
-- starting with the first occurrance up to (and excluding) the
-- second.  Implicitly calls 'dropDanglingReferences'.
shrinkScriptSM :: forall sid content . (Eq sid, Eq content, Ord sid, Show sid, Show content)
          => Script sid content -> [Script sid content]
shrinkScriptSM = nub . fmap (dropDanglingReferences . Script) . (shrink' siThisState) . scriptItems

shrink' :: forall i a . (Eq i, Ord i) => (a -> i) -> [a] -> [[a]]
shrink' mki xs = map f $ shortcuts (mki <$> xs)
  where
    f :: (Int, Int) -> [a]
    f (removeFrom, removeTo) =
        case splitAt removeFrom xs of
            (before, drop (removeTo - removeFrom) -> after) -> before ++ after

shortcuts :: (Ord i) => [i] -> [(Int, Int)]
shortcuts = join . map f . Map.elems . occurrances
  where
    f :: [Int] -> [(Int, Int)]
    f xs = [(i, j) | i <- xs, j <- xs, j > i]

occurrances :: (Ord i) => [i] -> Map i [Int]
occurrances = Map.fromListWith (++) . zipWith (\ i x -> (x, [i])) [0..]



-- * running SM-generated scripts

-- | Like 'runScript'', but use 'stateTraceInv' from 'SM' as
-- invariant.
runScriptSM :: forall sid content . (Show sid, Show content, Ord sid,
                                     JS.FromJSON content, JS.ToJSON content)
          => RunScriptSetup sid content
          -> SM sid content
          -> Script sid content
          -> IO (Trace sid content)
runScriptSM (RunScriptSetup verbose rootPath extractPath) (SM states) (Script items) =
    runScript' (RunScriptSetup verbose rootPath extractPath) (Script items) inv
  where
    inv traceItem trace = case stateTraceInv state of
            Nothing  -> Nothing
            Just inv -> inv traceItem trace
      where
        Just sid    = siThisState $ tiScriptItem traceItem
        Just state  = Map.lookup sid states



-- * rendering scripts in transition graphs.

-- | Generate GraphViz output (@.dot@-files).  (The 'SM' from which
-- the 'Script' has been generated needs to be provided for marking
-- terminal and start states and such.)
scriptToDot :: forall sid content . (Eq sid, Ord sid, Show sid, Show content)
            => String -> SM sid content -> Script sid content -> D.Graph
scriptToDot name machine script@(Script []) = error "transitionGraph: empty Script: not implemented."
scriptToDot name machine script@(Script (_:_)) = D.Graph D.UnstrictGraph D.DirectedGraph (Just (D.NameId name)) statements
  where
    nodes :: [State sid content]
    nodes = nubBy ((==) `on` stateId)
          . catMaybes
          . map (getState machine)
          . catMaybes
          $ siFromState (head is) : map siThisState is
      where
        is@(_:_) = scriptItems script

    statements :: [D.Statement]
    statements = (mkNode <$> nodes) ++ (mkEdge <$> scriptItems script)

    mkNode :: State sid content -> D.Statement
    mkNode s = D.NodeStatement (mkNodeId (stateId s)) . map (\ (k, v) -> D.AttributeSetValue (D.StringId k) (D.StringId v)) $
                 ("label",      cs . show . stateId $ s
                  ) :
                 ("fontsize",   cs $ show 10
                  ) :
                 ("style",      "filled"
                  ) :
                 ("shape",      case stateTerminal s of
                                  False -> "box"
                                  True  -> "egg"
                  ) :
                 ("fillcolor",  case stateStart s of
                                  True  -> "green"
                                  False -> "gray"
                  ) :
                 []

    mkNodeId :: sid -> D.NodeId
    mkNodeId sid = D.NodeId (D.NameId (show sid)) Nothing

    mkEdge :: (ScriptItem sid content) -> D.Statement
    mkEdge i@(ScriptItemHTTP { siFromState = Just fromState, siThisState = Just thisState }) = D.EdgeStatement entities attributes
      where
        entities = D.ENodeId D.NoEdge (mkNodeId fromState) :
                   D.ENodeId D.DirectedEdge (mkNodeId thisState) :
                   []
        attributes = map (\ (k, v) -> D.AttributeSetValue (D.StringId k) (D.StringId v)) $
                 ("label",      cs $ show (siSerial i) <> "\\l" <> show (siMethod i) <> "\\l" <> show (siHTTPPath i)
                  ) :
                 ("fontsize",   cs $ show 9
                  ) :
                 []


-- | ScriptItemHTTP constructor function.  Use this when writing 'SM'
-- machines.  Internals like serial numbers are initialized by
-- 'scriptFromSM'.
mkScriptItemHTTP :: RequestMethod -> Either SBS content
                 -> [(SBS, SBS)] -> [(SBS, SBS)] -> [(SBS, SBS)]
                 -> Either URI PathRef
                 -> ScriptItem sid content
mkScriptItemHTTP method body getparams postparams headers ref = ScriptItemHTTP
        { siSerial       = Ix (-1)
        , siFromState    = Nothing
        , siThisState    = Nothing
        , siMethod       = method
        , siBody         = body
        , siGetParams    = getparams
        , siPostParams   = postparams
        , siHeaders      = headers
        , siHTTPPath     = ref
        }


-- ** Graph algorithms

{-


FIXME: these are deprecated and don't work with the new data types.  remove them all?


-- | Keep the last state.  Find shortcuts in the earlier states that
-- maintain the validity of the trace (e.g., replace @2->3->1@ by
-- @2->1@ if @1@ is a direct successor of @2@ as well as one of @3@).
-- Output will only contain real shortcuts, not the unabbreviated
-- input trace.
--
-- FIXME: for now, this only cuts out proper cycles, not detours like
-- the example above.
shortcuts :: Script sid content -> [Script sid content]
shortcuts (Script xs) = error "" -- map (Script machine . reverse) . shortcuts' machine . reverse $ xs


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

-}
