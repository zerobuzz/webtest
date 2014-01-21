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
import Text.Show.Pretty

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
data ProtoState payload content =
    ProtoStateHTTP
      { psStart        :: Bool
      , psTerminal     :: Bool
      , psEnterHTTP    :: payload -> Gen (ScriptRq content)
      , psUpdateHTTP   :: forall payload' . payload' ~ payload => payload -> Either (NH.Response LBS) content -> payload'
      , psTest         :: forall payload' . payload' ~ payload => payload' -> QC.Property
      , psTransitions  :: [ProtoStateId]
      }
  | ProtoStateWD
      { psStart        :: Bool
      , psTerminal     :: Bool
      , psEnterWD      :: payload -> Gen (WD (Maybe JS.Value))
      , psUpdateWD     :: forall payload' . payload' ~ payload => payload -> Maybe JS.Value -> payload'
      , psTest         :: forall payload' . payload' ~ payload => payload' -> QC.Property
      , psTransitions  :: [ProtoStateId]
      }


instance Show (ProtoState payload content) where
    show (ProtoStateHTTP start term enter update test trans) =
        printf "(ProtoStateHTTP %s %s)" (show start) (show term)
    show (ProtoStateWD start term enter update test trans) =
        printf "(ProtoStateWD %s %s)" (show start) (show term)

type ProtoStateId = Int


protoStateHTTP :: (payload -> Gen (ScriptRq content))
          -> (forall payload' . payload' ~ payload => payload -> Either (NH.Response LBS) content -> payload')
          -> [ProtoStateId]
          -> ProtoState payload content
protoStateHTTP enter update trans = ProtoStateHTTP True True enter update (const $ mkprop True) trans


protoStateWD :: (payload -> Gen (WD (Maybe JS.Value)))
          -> (forall payload' . payload' ~ payload => payload -> Maybe JS.Value -> payload')
          -> [ProtoStateId]
          -> ProtoState payload content
protoStateWD enter update trans = ProtoStateWD True True enter update (const $ mkprop True) trans


data State payload content =
    StateHTTP
      { sEnterHTTP    :: ScriptRq content
      , sPayloadHTTP  :: payload
      }
  | StateWD
      { sEnterWD      :: WD (Maybe JS.Value)
      , sUpdateWD     :: payload
      }


instance (Show payload) => Show (State payload content) where
    show (StateHTTP _ pl) =
        printf "(StateHTTP %s)" (show pl)
    show (StateWD _ pl) =
        printf "(StateWD %s)" (show pl)


-- | A machine is a mapping from 'ProtoStateId's to 'ProtoState's.  All other
-- information is contained in the states.
data Machine payload content = Machine { fromMachine :: Map ProtoStateId (ProtoState payload content) }
  deriving (Show)


-- | Collect all 'ProtoState's with 'ProtoStateId' of a machine, and create it.
-- Crashes if any 'ProtoStateId' occurs more than once.
mkMachine :: [(ProtoStateId, ProtoState payload content)] -> Machine payload content
mkMachine states = if null dupeIds
                       then Machine $ Map.fromList states
                       else error $ "mkMachine: duplicate ProtoStateIds: " ++ show dupeIds
  where
    ids = map fst states
    dupeIds = ids \\ nub ids


-- | 'Machine' is an abstract specification of all sessions, or of
-- what could possibly ever happen.  The thing we want to implement
-- 'arbitrary' and 'shrink' on is one particular session at a time,
-- with all state transition events chronological order.  We call this
-- a 'Trace'.
data Trace payload content = Trace (Machine payload content) [ProtoStateId]
  deriving (Show)

-- | Like 'Trace', but contains the updated payload of each state
-- after execution.
data Trace' payload content = Trace' (Machine payload content) [(ProtoStateId, State payload content)]
  deriving (Show)


-- | Shrinking: This is currently just a call to 'shortcut'.  (Before
-- that, we should chop off the part of the script after the first
-- test failed, but this is done elsewhere.  FIXME: where?)
shrinkTrace :: Trace payload content -> [Trace payload content]
shrinkTrace = shortcuts


arbitraryTrace :: forall payload content . Machine payload content -> Gen (Trace payload content)
arbitraryTrace machine = mkTrace <$> (mkStart >>= sized . f)
  where
    mkStart :: Gen ProtoStateId
    mkStart = case Map.keys . Map.filter psStart . fromMachine $ machine of
                keys@(_:_) -> elements keys
                [] -> error $ "arbitraryTrace: machine without start state: " ++ ppShow machine

    mkTrace :: Maybe [ProtoStateId] -> Trace payload content
    mkTrace (Just ids) = Trace machine ids
    mkTrace Nothing = error $ "arbitraryTrace: machine without terminator state: " ++ ppShow machine

    f :: ProtoStateId -> Int -> Gen (Maybe [ProtoStateId])
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


-- | (Could also be called 'runTrace'?)
arbitraryTrace' :: forall payload content . Machine payload content -> Gen (Trace' payload content)
arbitraryTrace' = error "wef"



{-

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
    f :: [ProtoStateId] -> IO QC.Property
    f (idThis : idsRest) = do
        debugM "" $ "propTrace': " ++ show idThis

        let stateThis :: ProtoState
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
shortcuts' :: Machine payload content -> [ProtoStateId] -> [[ProtoStateId]]
shortcuts' machine [] = []
shortcuts' machine [only] = []
shortcuts' machine (last:before@(second:before')) = (last:) <$> (direct before' ++ recursive)
  where
    direct :: [ProtoStateId] -> [[ProtoStateId]]
    direct [] = []
    direct candidate@(x:xs) = (if x == second then (candidate:) else id) $ direct xs

    recursive :: [[ProtoStateId]]
    recursive = shortcuts' machine before


-- | Produce all possible direct successors (connected via a single
-- transition) of a state.
successors :: Machine payload content -> ProtoStateId -> [ProtoStateId]
successors machine id = maybe [] psTransitions $ Map.lookup id $ fromMachine machine

-- | Produce all descentants of a state, ie. both direct successors
-- and their successors, recursively.  Each descentant is annotated
-- with the length of the path that it was reached on in the search
-- (not necessarily the length of the shortest path to it!).  Output
-- list is infinite iff there are cycles in the machine graph.
descendants :: Machine payload content -> ProtoStateId -> [(Int, ProtoStateId)]
descendants machine id = map (1,) direct ++ concat (map recursive direct)
  where
    direct = successors machine id
    recursive id' = map (first (+1)) $ descendants machine id'

-- | Like 'successors', but up the transition graph.  (In the current
-- implementation, this is a bit more expensive than down.)
predecessors :: Machine payload content -> ProtoStateId -> [ProtoStateId]
predecessors machine id = map fst . filter hit . Map.toList . fromMachine $ machine
  where
    hit (_, state) = id `elem` psTransitions state

-- | Like 'descendants' to 'successors', but calls 'predecessors'
-- instead.
ancestors :: Machine payload content -> ProtoStateId -> [(Int, ProtoStateId)]
ancestors machine id = map (1,) direct ++ concat (map recursive direct)
  where
    direct = predecessors machine id
    recursive id' = map (first (+1)) $ ancestors machine id'

-- | FIXME: 'descendants' and 'shortestTerminatingTrace' should both
-- make use of more general graph algorithms.  Check king paper and
-- ghc libs once more!
shortestTerminatingTrace :: Machine payload content -> ProtoStateId -> Maybe [ProtoStateId]
shortestTerminatingTrace machine start = listToMaybe $ step [] start
  where
    step :: [ProtoStateId] -> ProtoStateId -> [[ProtoStateId]]
    step visited now
        | now `elem` visited  = []        -- (give up when hitting a cycle.)
        | terminal now        = [[now]]   -- (end when hitting a terminal state.)
        | True                = let recurse :: [[ProtoStateId]]
                                    recurse = concatMap (step (now:visited)) (successors machine now)
                                in sieve ((now:) <$> recurse)

    terminal :: ProtoStateId -> Bool
    terminal sid = case Map.lookup sid (fromMachine machine) of Just s -> psTerminal s

    sieve :: [[ProtoStateId]] -> [[ProtoStateId]]
    sieve = map snd
          . sortBy (flip compare `on` fst)
          . map (\ ids -> (length ids, ids))
