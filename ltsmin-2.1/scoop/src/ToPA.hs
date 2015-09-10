module ToPA where

import Expressions
import LPPE
import Data.List
import DataSpec 
import Processes
import Confluence
import Auxiliary
import Debug.Trace
import Data.HashMap
import Data.Maybe
import StepPA

type TransitionInts    = (Int, RewardString, EdgeLabel, [(Probability, Int)])
type Statespace        = ([State], [Transition]) 
type StatespaceInts    = ([State], [TransitionInts]) 
type RepMapInts        = (Map State Int, [TransitionInts])
type Visited           = (Int, Int)
type RepresentationMap = Map State State

-------------------------------------------------
-- General functions to deal with state spaces --
-------------------------------------------------

-- Given a state space, provides the set of states
getStates :: StatespaceInts -> [State]
getStates (states, transitions) = states

-- Given a state space, provides the set of transitions
getTransitions :: StatespaceInts -> [TransitionInts]
getTransitions (states, transitions) = transitions


-------------------------------------------------
-- Functions for generation of the state space --
-------------------------------------------------

-- Given a system and a set of confluent summands, 
-- this function provides the number of states and transitions.
getStatesAndTransitionsConfluence :: Bool -> PSpecification -> ConfluentSummands -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> [String] -> ((Int, Int), (Int, Int))
getStatesAndTransitionsConfluence ignorecycles spec confluent checkConfluence checkVisited checkDTMC isMA removeRates preserveDivergence showDeadlocks storeReps reachActions = ((numberOfStates,numberOfTransitions), visited)
  where
    (statespace, initial, visited) = getStateSpace ignorecycles spec confluent checkConfluence checkVisited checkDTMC isMA removeRates preserveDivergence showDeadlocks storeReps reachActions
    numberOfStates                 = size (fst statespace) 
    numberOfTransitions            = length [t | t <- snd statespace, thd4 t /= "reachConditionAction", take 17 (thd4 t) /= "stateRewardAction"]

-- Given a system and a set of confluent summands,
-- this function provides the unfolded state space.
getStateSpace :: Bool -> PSpecification -> ConfluentSummands -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> [String] -> (RepMapInts, State, (Int, Int))
getStateSpace ignorecycles spec confluent checkConfluence checkVisited checkDTMC isMA removeRates preserveDivergence showDeadlocks storeReps reachActions = (statespace, init, (fst visited2 + visitedStates, snd visited2 + visitedTrans))
  where
    (init, repr,visitedStates, visitedTrans) = if checkConfluence 
	                                           then getRepresentative spec (empty) confluent (getInitialState spec) storeReps reachActions
                                               else (getInitialState spec, empty, 1, 0)
    (statespace, visited2) = if   not(checkVisited) then
	                                         (getStateSpace2 spec [init] 0 (singleton init 0, []) repr confluent checkConfluence checkDTMC isMA removeRates preserveDivergence showDeadlocks storeReps reachActions, (0,0))
                                        else getStateSpace2Visited spec [init] 0 (singleton init 0, []) repr (0 ,0) confluent checkConfluence checkDTMC isMA removeRates preserveDivergence showDeadlocks storeReps reachActions

-- This function takes an LPPE, a set of states to explore, the state space
-- that was generated so far, and a set of confluent summands.
getStateSpace2 :: PSpecification -> [State] -> Int -> RepMapInts -> RepresentationMap -> ConfluentSummands -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> [String] -> RepMapInts
getStateSpace2 spec []             highest statespace repr confluent checkConfluence checkDTMC isMA removeRates preserveDivergence showDeadlocks storeReps reachActions = statespace
getStateSpace2 spec (state:states) highest statespace repr confluent checkConfluence checkDTMC isMA removeRates preserveDivergence showDeadlocks storeReps reachActions
  | forceEvaluation = error("")
  | checkDTMC && length(newTransitions) > 1 = error("The model is not deterministic in state " ++ show state ++ ".\nThe enabled actions are: " ++ show [b | (a,r,b,c) <- newTransitions]) 
  | otherwise       = getStateSpace2 spec toExplore highest2 updatedStatespace newRepr confluent checkConfluence checkDTMC isMA removeRates preserveDivergence showDeadlocks storeReps reachActions
  where
    ((newStates, newTransitions), 
      newRepr, _)                  = exploreState spec state state repr confluent checkConfluence False removeRates preserveDivergence showDeadlocks storeReps reachActions
    (statesSeen, transitionsSeen)  = statespace
    reallyNewStates                = findReallyNewStates (nub newStates) statesSeen
    reallyNewTransitions           = if isMA then mergeTransitions newTransitions else nub newTransitions
    (updatedstates, highest2)      = addListToMap reallyNewStates highest statesSeen
    optimisedTransitions           = Data.List.map (changeTransition updatedstates) reallyNewTransitions
    updatedtransitions             = (optimisedTransitions ++ transitionsSeen)
    updatedStatespace              = (updatedstates, updatedtransitions)
    toExplore                      = (reallyNewStates ++ states)
    forceEvaluation                = [0 | (s,r,a,t) <- optimisedTransitions, s < -1 || length a < -1 || or [length a < -1 || b < -1 | (a,b) <- t]] /= []

-- This function takes an LPPE, a set of states to explore, the state space
-- that was generated so far, and a set of confluent summands.
getStateSpace2Visited :: PSpecification -> [State] -> Int -> (Map State Int, [TransitionInts]) -> RepresentationMap -> Visited -> ConfluentSummands -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> [String] -> (RepMapInts, (Int, Int))
getStateSpace2Visited spec []             highest statespace repr visited [] checkConfluence checkDTMC isMA removeRates preserveDivergence showDeadlocks storeReps reachActions
  = (statespace, (size (fst statespace) - 1, length transitions))
  where
    transitions = snd statespace
getStateSpace2Visited spec []             highest statespace repr visited confluent checkConfluence checkDTMC isMA removeRates preserveDivergence showDeadlocks storeReps reachActions = (statespace, visited)
getStateSpace2Visited spec (state:states) highest statespace repr visited confluent checkConfluence checkDTMC isMA removeRates preserveDivergence showDeadlocks storeReps reachActions
  | forceEvaluation = error("")
  | otherwise       = getStateSpace2Visited spec toExplore highest2 updatedStatespace newRepr newVisited2 confluent checkConfluence checkDTMC isMA removeRates preserveDivergence showDeadlocks storeReps reachActions
  where
    ((newStates, newTransitions), 
     newRepr, newVisited1)         = exploreState spec state state repr confluent checkConfluence True removeRates preserveDivergence showDeadlocks storeReps reachActions
    (statesSeen, transitionsSeen)  = statespace
    reallyNewStates                = findReallyNewStates (nub newStates) statesSeen
    reallyNewTransitions           = if isMA then mergeTransitions newTransitions else nub newTransitions
    (updatedstates, highest2)      = addListToMap reallyNewStates highest statesSeen
    optimisedTransitions           = Data.List.map (changeTransition updatedstates) reallyNewTransitions
    updatedtransitions             = (optimisedTransitions ++ transitionsSeen)
    updatedStatespace              = (updatedstates, updatedtransitions)
    toExplore                      = (reallyNewStates ++ states)
    visitedTransitions             = snd newVisited1 + snd visited
    visitedStates                  = fst newVisited1 + fst visited --addListToMap2 updatedstates (fst newVisited1) (fst visited)
    newVisited2                    = (visitedStates, visitedTransitions)
    forceEvaluation                = visitedStates < -1 || visitedTransitions < -1 || [0 | (s,r,a,t) <- optimisedTransitions, s < -1 || length a < -1 || or [length a < -1 || b < -1 | (a,b) <- t]] /= []

mergeTransitions :: [Transition] -> [Transition]
mergeTransitions []                   = []
mergeTransitions ((from,reward,label,to):ts) | take 17 label == "stateRewardAction" = (from, reward, label,to):(mergeTransitions ts)
                                             | take 5 label == "rate(" = (from, reward, "rate(" ++ totalRate ++ ")", to):newTS
	                                         | otherwise               = (from, reward, label,to):newTS2
  where
    totalRate   = writeFraction (sum [getFraction (takeWhile (/= ')') (drop 5 l)) | (f,r,l,t) <- ((from,reward,label,to):ts), f == from, t == to, take 5 l == "rate("])
    newTS     = [(f,r,l,t) | (f,r,l,t) <- ts, f /= from || t /= to	 || take 5 l /= "rate("]
    newTS2    = mergeTransitions [(f,r,l,t) | (f,r,l,t) <- ts, f /= from || l /= label || t /= to || r /= reward]

changeTransition :: Map State Int -> Transition -> TransitionInts
changeTransition states (i, r, a, next) = (fromJust (Data.HashMap.lookup i states), r, a, 
                                       Data.List.map (\x -> (fst x, fromJust (Data.HashMap.lookup (snd x) states))) next)

findReallyNewStates :: [State] -> Map State Int -> [State]
findReallyNewStates []     seen = []
findReallyNewStates (x:xs) seen | member x seen = findReallyNewStates xs seen
                                | otherwise     = x:(findReallyNewStates xs seen)

addListToMap :: [State] -> Int -> Map State Int -> (Map State Int, Int)
addListToMap [] highest seen     = (seen, highest)
addListToMap (x:xs) highest seen = addListToMap xs (highest + 1) (Data.HashMap.insert x (highest + 1) seen)

addListToMap2 :: Map State Int -> [State] -> Map State Int -> Map State Int
addListToMap2 seen [] visited     = visited
addListToMap2 seen (x:xs) visited | member x seen || member x visited = addListToMap2 seen xs visited
                                  | otherwise                         = addListToMap2 seen xs (Data.HashMap.insert x 0 visited)

-- sourceState geeft aan welke state als source-state in de transities moet komen te staan.
-- Als je ctau-stappen hebt dan ga je de toestand waar die uitkomt zelf niet in de statespace zetten, 
-- maar de concatenatie van de transitie van sourceState naar from en de transities vanuit from
exploreState :: PSpecification -> State -> State -> RepresentationMap -> ConfluentSummands -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> [String] -> (Statespace, RepresentationMap, (Int, Int))
exploreState spec from sourceState repr confluent checkConfluence countVisited removeRates preserveDivergence showDeadlocks storeReps reachActions
  | if showDeadlocks && (length transNoDivergence == 0 && (not(preserveDivergence) || length confluentBehaviour == 0)) 
	then trace("Deadlock in state " ++ show sourceState) not countVisited 
	else not countVisited = (newTransitions, newRepr, (0,0)) 
  | otherwise        = (newTransitions, newRepr, (visitedStates,visitedTransitions + visitedTrans))
  where
    (LPPE name params summands)     = getLPPE spec
    mapping                         = zip (Data.List.map fst params) from -- (Data.List.map snd params) from
    confluentBehaviour              = potentialBehaviour False spec confluent (getDataSpec spec) sourceState mapping [summands!!i | i <- [0.. length summands - 1], elem i confluent]
--    fakeConfluentBehaviour           = [(state,label,[("1", next)]) | (state,label,[("1", next)]) <- confluentBehaviour_, enablesObservableAction spec next reachActions && not(enablesObservableAction spec state reachActions)]
--    confluentBehaviour               = [t | t <- confluentBehaviour_, not (elem t fakeConfluentBehaviour)]
    transNoDivergence                = potentialBehaviour False spec confluent (getDataSpec spec) sourceState mapping [summands!!i | i <- [0.. length summands - 1], not (elem i confluent)]
--                                       ++ fakeConfluentBehaviour
    trans__                         = if preserveDivergence && length confluentBehaviour > 0 then [(sourceState, "0", "tau",[("1",sourceState)])] ++ transNoDivergence else transNoDivergence
    trans_                          = if (not(removeRates)) then trans__ else 
	                                      (if hasTau trans__ then removeRateTransitions trans__ else trans__)
    visitedTransitions              = length trans_
    (trans, newRepr, 
     visitedStates1, visitedTrans)  = if checkConfluence then changeToRepresentatives spec repr confluent trans_ storeReps reachActions
	                                                     else (trans_, repr, 0, 0)
    states_                         = concat [Data.List.map snd list | (s,r,a,list) <- trans]
    states                          = if and (Data.List.map (checkStates spec) states_) then states_ else error("Error in ToPA")
    newTransitions                  = (states, trans)
    visitedStates                   = visitedStates1 -- + 1 --states ++ (concat [List.map snd list | (s,a,list) <- trans_]) ++ visitedStates1

checkStates spec state = if (and correct) then True else error("Type error in state " ++ show state ++ ". Value " ++ (state!!index) ++ " is not of type " ++ getTypeName2 (snd (pars!!index))
                                                                  ++ if getTypeName2 (snd (pars!!index)) == "Queue" then " (since not all elements are values of a known type)." else ".")
  where
    pars     = getLPPEPars (getLPPE spec)
    dataspec = getDataSpec spec
    correct  = [correctValue dataspec (state!!i) (snd (pars!!i)) | i <- [0..length state - 1]]
    index    = [i | i <- [0..length state - 1], correct!!i == False]!!0

changeToRepresentatives :: PSpecification -> RepresentationMap -> ConfluentSummands -> [Transition] -> Bool -> [String] -> ([Transition], RepresentationMap, Int, Int)
changeToRepresentatives _ repr _ [] storeReps reachActions = ([], repr, 0, 0)
changeToRepresentatives spec repr confluent (t:rest) storeReps reachActions = (newT:newRest, newRepr2, visited1 + visited2, visitedTrans1 + visitedTrans2)
  where
    (newT, newRepr1,visited1,visitedTrans1) = changeTransitionToRepresentatives spec repr confluent t storeReps reachActions
    (newRest, newRepr2,visited2, visitedTrans2) = changeToRepresentatives spec newRepr1 confluent rest storeReps reachActions

changeTransitionToRepresentatives :: PSpecification -> RepresentationMap -> ConfluentSummands -> Transition -> Bool -> [String] -> (Transition, RepresentationMap, Int, Int)
changeTransitionToRepresentatives spec repr confluent (s,r,a,next) storeReps reachActions = ((newS, r, a, newNext), newRepr2, visited, visitedTrans)
  where
    newS     = s
    (newNext, newRepr2, visited, visitedTrans) = changeNextStateToRepresentatives spec repr confluent next storeReps reachActions

changeNextStateToRepresentatives :: PSpecification -> RepresentationMap -> ConfluentSummands -> [(Probability, State)] -> Bool -> [String] -> ([(Probability, State)], RepresentationMap, Int, Int)
changeNextStateToRepresentatives spec repr confluent [] storeReps reachActions = ([], repr, 0, 0)
changeNextStateToRepresentatives spec repr confluent ((p,s):rest) storeReps reachActions = ((p,newS):newRest, newRepr2, visited1 + visited2, visitedTrans1 + visitedTrans2)
  where
    (newS, newRepr1, visited1, visitedTrans1)    = getRepresentative spec repr confluent s storeReps reachActions
    (newRest, newRepr2, visited2, visitedTrans2) = changeNextStateToRepresentatives spec newRepr1 confluent rest storeReps reachActions

---------------------
-- Representatives --
---------------------

type NumbersTable = [(State, Int)]
type ListTable    = [(State, [State])]
type StateTable  = [(State, State)]

getRepresentative spec repr confluent state storeReps reachActions
  | representative /= Nothing = (fromJust representative, repr, 0, 0) 
  | otherwise = (newRepresentative, newRepresentatives, length visited, visitedTrans)
  where
    numbers                                   = setItem [] state 0
    representative                            = Data.HashMap.lookup state repr
    (newRepresentative,visited, visitedTrans) = getRepresentative2 repr spec confluent state numbers [] [] [] 0 reachActions
 
  --ONLY STORE STATES THAT ARE A REPRESENTATIVE THEMSELVES:  
  --  newRepresentatives                       = Data.HashMap.insert newRepresentative newRepresentative repr

 -- ONLY STORE REPRESENTATIVES FOR STATES THAT HAVE BEEN ASKED FOR THEIR REPRESENTATIVE 
  --  newRepresentatives                       = Data.HashMap.insert state newRepresentative repr  


 --   STORE EVERYTHING:
   -- newRepresentatives_                       = Data.HashMap.insert newRepresentative newRepresentative repr  
    newRepresentatives                        = if (not storeReps) 
 	                                            then Data.HashMap.insert newRepresentative newRepresentative repr -- repr <- niks opslaan 
 	                                            else --Data.HashMap.insert newRepresentative newRepresentative (addBlaat newRepresentatives_ newRepresentative visited)
                                                     addBlaat repr newRepresentative visited
 --   nrVisited                                 = if newRepresentative == state then 0 else length visited

addBlaat newRepresentatives representative [] = newRepresentatives
addBlaat newRepresentatives representative (s:ss) = addBlaat (Data.HashMap.insert s representative  newRepresentatives) representative ss 

getRepresentative2 repr spec confluent state numbers low unexplored backtrack count reachActions
  | already   = (existingRepresentative, visited1, visitedTrans1)
  | found     = (findLowestRepresentative state low, visited1, visitedTrans1)
  | otherwise = (rep, visited1 ++ visited2, visitedTrans1 + visitedTrans2)
  where
    newState      = getNumberFromList numbers state == 0
    (numbers2, low2, unexplored2, count2, visited1, already, existingRepresentative, visitedTrans1) 
             = if newState then exploreNewState repr spec confluent state numbers low unexplored count reachActions
                           else (numbers, low, unexplored, count, [], False, [], 0)
    deadlockState = getItem unexplored2 state == []
    (found, low3, state2)                          = if deadlockState      then exploreDeadlockState state numbers2 unexplored2 low2 backtrack  
                 	                                                       else (False, low2, state)
    (unexplored3, backtrack2, state3, low4)        = if not(deadlockState) then getRepresentative3 unexplored2 numbers2 backtrack state low2
	                                                                       else (unexplored2, backtrack, state2, low3)
    (rep, visited2, visitedTrans2) = getRepresentative2 repr spec confluent state3 numbers2 low4 unexplored3 backtrack2 count2 reachActions

exploreNewState :: RepresentationMap -> PSpecification -> ConfluentSummands -> State -> NumbersTable -> NumbersTable -> ListTable -> Int -> [String] -> (NumbersTable, NumbersTable, ListTable, Int, [State], Bool, State, Int)
exploreNewState repr spec confluents v numbers low unexplored count reachActions = (numbers3, low2, unexplored3, count2, [v], already, existing, visitedTransitions)
  where
    count2                      = count + 1
    numbers2                    = setItem numbers v count2
    low2                        = setItem low v count2
    unexplored2                 = setItem unexplored v []
    (LPPE name params summands) = getLPPE spec
    mapping                     = zip (Data.List.map fst params) v -- (Data.List.map snd params) v
    trans                      = potentialBehaviour True spec confluents (getDataSpec spec) v mapping [summands!!i | i <- [0.. length summands - 1], elem i confluents]
--    fakeConfluence              = [(state,label,[("1", next)]) | (state,label,[("1", next)]) <- trans_, (enablesObservableAction spec next reachActions && not(enablesObservableAction spec state reachActions))]
--    trans                       = [t | t <- trans_, not (elem t fakeConfluence)]
    -- Het onderstaande is zodat, in geval van een state met slechts 1 uitgaande tau-transitie, die ook als confluent gezien wordt
    transNonConfluent           = potentialBehaviour True spec confluents (getDataSpec spec) v mapping [summands!!i | i <- [0.. length summands - 1], not (elem i confluents)]
--                                  ++ fakeConfluence
    transNonConfluentInteractive = [(a,r, b,c) | (a,r, b,c) <- transNonConfluent, take 4 b /= "rate"]
    newstates                   = if (trans == [] && length transNonConfluentInteractive == 1 && 
	                              length [(state,reward,label,[("1", next)]) | (state,reward, label,[("1", next)]) <- transNonConfluentInteractive, not (enablesObservableAction spec next reachActions && not(enablesObservableAction spec state reachActions))] == 1
	                              && thd4 (transNonConfluentInteractive!!0) == "tau" && snd4 (transNonConfluentInteractive!!0) == "0" && length (frt4 (transNonConfluentInteractive!!0)) == 1) 
	                              then concat [Data.List.map snd next | (s,r,a,next) <- transNonConfluentInteractive] 
	                              else concat [Data.List.map snd next | (s,r,a,next) <- trans]   
    --newstates                   = concat [Data.List.map snd next | (s,a,next) <- trans]
    --------------
    (unexplored3, numbers3, already, existing) = addTransitions repr v unexplored2 numbers2 newstates
    visitedTransitions          = length trans

addTransitions :: RepresentationMap -> State -> ListTable -> NumbersTable -> [State] -> (ListTable, NumbersTable, Bool, State)
addTransitions repr v unexplored numbers []     = (unexplored, numbers, False, [])
addTransitions repr v unexplored numbers (t:ts) | existing /= Nothing = (unexplored, numbers, True, fromJust existing)
                                                | otherwise = addTransitions repr v unexplored2 numbers2 ts
  where
    unexplored2 = setItem unexplored v (t:(getItem unexplored v))
    numbers2    = if getNumberFromList numbers t == -1 then setItem numbers t 0 else numbers
    existing    = Data.HashMap.lookup t repr

exploreDeadlockState :: State -> NumbersTable -> ListTable -> NumbersTable -> StateTable -> (Bool, NumbersTable, State)
exploreDeadlockState v numbers unexplored low backtrack | found     = (True, [], [])
                                                        | otherwise = (False, low2, v2)
  where
    lowV  = getNumberFromList low v
    found = getNumberFromList numbers v == lowV 
    prev  = getItem backtrack v
    low2  = setItem low prev (min (getNumberFromList low prev) lowV)
    v2    = prev

getRepresentative3 :: ListTable -> NumbersTable -> StateTable -> State -> NumbersTable -> (ListTable, StateTable, State, NumbersTable)
getRepresentative3 unexplored numbers backtrack v low = (unexplored2, backtrack2, v2, low2)
  where
    rest        = getItem unexplored v
    u           = head rest
    unexplored2 = setItem unexplored v (tail rest)
    nrU         = getNumberFromList numbers u
    dosomething = nrU == 0
    backtrack2  = if dosomething then setItem backtrack u v else backtrack
    v2          = if dosomething then u else v
    low2        = if not(dosomething) && nrU < (getNumberFromList numbers v) then setItem low v (min (getNumberFromList low v) (getNumberFromList low u)) else low

findLowestRepresentative v low = foldl min v options
  where
    options = [state | (state, nr) <- low, nr == getNumberFromList low v]

setItem :: [(State, a)] -> State -> a -> [(State, a)]
setItem [] v i                           = [(v,i)]
setItem ((val, nr):rest) v i | val == v  = (val, i):rest
                               | otherwise = (val,nr):(setItem rest v i)

getItem :: [(State, a)] -> State -> a
getItem ((v,i):rest) from | from == v = i
                            | otherwise = getItem rest from

getNumberFromList :: NumbersTable -> State -> Int
getNumberFromList [] s = -1
getNumberFromList ((v,i):rest) from | from == v = i
                                    | otherwise = getNumberFromList rest from

hasTau :: [Transition] -> Bool
hasTau [] = False
hasTau ((from,reward,label,next):rest)  = (take 4 label /= "rate" && label /= "reachConditionAction" && take 17 label /= "stateRewardAction") || hasTau rest
--hasTau ((from,label,next):rest)  = label == "tau" || hasTau rest

removeRateTransitions :: [Transition] -> [Transition]
removeRateTransitions [] = []
removeRateTransitions ((from,reward,label,next):rest) | take 4 label == "rate" = removeRateTransitions rest
                                               | otherwise              = (from,reward,label,next):(removeRateTransitions rest)

------------------------
-- Invisibility check --
------------------------

enablesObservableAction :: PSpecification -> State -> [String] -> Bool
enablesObservableAction spec state reachActions = observableTransitions
  where
    (LPPE name params summands) = getLPPE spec
    mapping                     = zip (Data.List.map fst params) state
    observableTransitions       = summandIsEnabled spec (getDataSpec spec) state mapping [summands!!i | i <- [0.. length summands - 1], elem (getAction (summands!!i)) reachActions]

summandIsEnabled :: PSpecification -> DataSpec -> State -> Mapping -> [PSummand] -> Bool
summandIsEnabled spec dataspec sourceState mapping []     = False
summandIsEnabled spec dataspec sourceState mapping (s:ss) = current || rest
  where
    current = summandIsEnabled2 spec dataspec sourceState mapping s
    rest    = summandIsEnabled  spec dataspec sourceState mapping ss

summandIsEnabled2 :: PSpecification -> DataSpec -> State -> Mapping -> PSummand -> Bool
summandIsEnabled2 spec dataspec sourceState mapping (((var,sort):params), c, reward, a, aps, probChoices, g)
  = unfoldSummationEnabled spec dataspec sourceState mapping (var,sort) (getValues dataspec sort) (params, c, reward, a, aps, probChoices, g)
summandIsEnabled2 spec dataspec sourceState mapping ([], c, reward, a, aps, probChoices, g) = executable
  where
    executable  = evalExpr (thd3 dataspec) mapping c == "T"

unfoldSummationEnabled :: PSpecification -> DataSpec -> State -> Mapping -> (Variable,Type) -> [Constant] -> PSummand -> Bool
unfoldSummationEnabled spec dataspec sourceState mapping (var,sort) [] summand = False
unfoldSummationEnabled spec dataspec sourceState mapping (var,sort) (value:values) summand = current || rest
  where
    mapping2 = updateMapping mapping var value
    current  = summandIsEnabled2 spec dataspec sourceState mapping2 summand
    rest     = unfoldSummationEnabled spec dataspec sourceState mapping (var,sort) values summand
