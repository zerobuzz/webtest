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

{-# LANGUAGE FlexibleContexts, TypeFamilies, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, PackageImports #-}

{-  OPTIONS -fwarn-unused-imports #-}

-- | FIXME: (1) update documentation.  (2) this is about state
-- machines that mix both HTTP 'ScriptRq's / 'StoryItem's and a
-- webdriver equivalent; this means we need to change types 'Script'
-- and 'Story'.  for now, this module is only about HTTP.
module Test.WebApp.StateMachines
where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Function
import Data.List as List
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import Data.String.Conversions
import Data.Typeable
import GHC.Generics
import Language.Dot as D
import Network.URI
import Prelude hiding ((++))
import Test.QuickCheck as QC
import Test.WebDriver
import Test.WebDriver.Monad
import Text.Printf
import Text.Show.Pretty

import "mtl" Control.Monad.Trans

import qualified Data.Aeson as JS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Network.HTTP as NH

import Test.WebApp.HTTP.Story hiding (Script (..), Story (..))
import Test.WebDriver.Missing
import Test.QuickCheck.Missing
import Test.WebApp.WebDriver hiding (State (..), StateId, Machine (..), Trace (..),
                                     propTrace, propTrace', predecessors, successors, ancestors, descendants,
                                     shortcuts, shortcuts', shortestTerminatingTrace, arbitraryTrace)
                               -- (all the above will eventually be merged into here.)



{-
import Test.WebDriver.Capabilities
import Test.WebDriver.Classes
import Test.WebDriver.Commands
-- import Test.WebDriver.Internal

import Control.Applicative
import Control.Exception.Lifted
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (liftM)
import Control.Monad.Trans.Control (MonadBaseControl(..), StM)
import "MonadCatchIO-transformers" Control.Monad.CatchIO (MonadCatchIO)
import "monads-tf" Control.Monad.State.Strict (StateT, MonadState, evalStateT, get, put)
import "monads-tf" Control.Monad.Trans (lift)
-}



-- * State machines

data SM sid content = SM { fromSM :: Map sid (State sid content) }
  deriving (Show)

data State sid content =
      State
        { stateId           :: sid
        , stateStart        :: Bool
        , stateTerminal     :: Bool
        , stateTransitions  :: [Script sid content -> Gen (ScriptItem sid content, sid)]
        }
  deriving (Typeable)

instance (Show sid) => Show (State sid content) where show s = "<State " <> show (stateId s) <> ">"

newtype Script sid content = Script { scriptItems :: [ScriptItem sid content] }
  deriving (Show, Typeable)

instance Monoid (Script sid content) where
  mappend (Script xs) (Script ys) = Script $ xs ++ ys
  mempty = Script []

newtype Trace sid content = Trace { traceItems  :: [TraceItem sid content] }
  deriving (Typeable)

data ScriptItem sid content =
      ScriptItemHTTP
        { siSerial       :: Ix
        , siFromState    :: Maybe (State sid content)
        , siThisState    :: Maybe (State sid content)
        , siMethod       :: NH.RequestMethod
        , siBody         :: Either SBS content
        , siGetParams    :: [(SBS, SBS)]
        , siPostParams   :: [(SBS, SBS)]
        , siHeaders      :: [(SBS, SBS)]
        , siHTTPPath     :: Either URI IxRef
        }
{-
    | ScriptItemDWD
        { siSerial       :: Ix
        , siDWD          :: (MonadIO wd, WebDriver wd) => wd (Either Element content)
        }
-}
  deriving (Show, Typeable, Generic)

data TraceItem sid content =
      TraceItemHTTP
        { tiScriptItem :: ScriptItem sid content
        , tiEffectHTTP :: Maybe (NH.Response LBS)
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
                            let thisItem' = thisItem { siSerial     = length (scriptItems script)
                                                     , siFromState  = Just lastState
                                                     , siThisState  = Just thisState
                                                     }
                                Just thisState = Map.lookup thisSid $ fromSM machine

                            vectorOf w2 (mkItem (size - 1) (script <> Script [thisItem']) thisState)
                        ) transitions

            return $ shortestScript scripts

        preferTerminals :: [(ScriptItem sid content, sid)] -> [(ScriptItem sid content, sid)]
        preferTerminals = sortBy (\ a b -> f (siThisState $ fst a) (siThisState $ fst b))
            where
                f :: Maybe (State sid content) -> Maybe (State sid content) -> Ordering
                f (Just (stateTerminal -> True)) (Just (stateTerminal -> False)) = EQ
                f (Just (stateTerminal -> True)) (Just _) = GT
                f _ _ = EQ

        shortestScript :: [Script sid content] -> Script sid content
        shortestScript = f Nothing
            where
                f _         (x:_)  | length (scriptItems x) < 1                       = error "arbitraryScript: internal error."
                f _         (x:_)  | length (scriptItems x) == 1                      = x
                f (Just x') (x:xs) | length (scriptItems x) < length (scriptItems x') = f (Just x) xs
                f (Just x') (x:xs)                                                    = f (Just x') xs
                f Nothing   [x]                                                       = x
                f (Just x') []                                                        = x'

                f acc rest = error $ "arbitraryScript.shortestScript: unmatched pattern: " ++ show (acc, rest)


prop_scriptFromSM_serials :: QC.Property
prop_scriptFromSM_serials = forAll (scriptFromSM example2) $ mkprop . and . zipWith (==) [0..] . map siSerial . scriptItems


-- | default shrink for Scripts
shrinkScript :: forall sid content . SM sid content -> Script sid content -> [Script sid content]
shrinkScript machine script = error "wef"


transitionGraph :: forall sid content . (Eq sid, Show sid) => String -> Script sid content -> Int
transitionGraph = error "wef"


-- | FIXME: show transition http (or wd) request.
scriptToDot :: forall sid content . (Eq sid, Ord sid, Show sid, Show content)
            => String -> Script sid content -> D.Graph
scriptToDot name script@(Script []) = error "transitionGraph: empty Script: not implemented."
scriptToDot name script@(Script (_:_)) = D.Graph D.UnstrictGraph D.DirectedGraph (Just (D.NameId name)) statements
  where
    nodes :: [State sid content]
    nodes = nubBy ((==) `on` stateId) . catMaybes $ siFromState (head is) : map siThisState is
      where
        is@(_:_) = scriptItems script

    statements :: [D.Statement]
    statements = (mkNode <$> nodes) ++ (mkEdge <$> scriptItems script)

    mkNode :: State sid content -> D.Statement
    mkNode s = D.NodeStatement (mkNodeId s) . map (\ (k, v) -> D.AttributeSetValue (D.StringId k) (D.StringId v)) $
                 ("label",      cs . show . stateId $ s
                  ) :
                 ("shape",      "box"
                  ) :
                 ("color",      case stateTerminal s of
                                  False -> "black"
                                  True  -> "red"
                  ) :
                 ("fillcolor",  case stateStart s of
                                  True  -> "green"
                                  False -> "white"
                  ) :
                 []

    mkNodeId :: State sid content -> D.NodeId
    mkNodeId s = D.NodeId (D.NameId (show (stateId s))) Nothing

    mkEdge :: (ScriptItem sid content) -> D.Statement
    mkEdge i@(ScriptItemHTTP { siFromState = Just fromState, siThisState = Just thisState }) = D.EdgeStatement entities attributes
      where
        entities = D.ENodeId D.NoEdge (mkNodeId fromState) :
                   D.ENodeId D.DirectedEdge (mkNodeId thisState) :
                   []
        attributes = map (\ (k, v) -> D.AttributeSetValue (D.StringId k) (D.StringId v)) $
                 ("label",      cs $ show (siSerial i, siMethod i, siHTTPPath i)
                  ) :
                 []


-- ** Graph algorithms

{-

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
