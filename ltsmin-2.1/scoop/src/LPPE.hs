module LPPE where

import Processes
import DataSpec
import Expressions
import Auxiliary
import Data.List
import Debug.Trace
import qualified ParserExpressionsGlobals
import ParserAux
import Parser

type PSystem        = (LPPE, InitialState)
type PSpecification = (LPPE, InitialState, DataSpec)
type Constant      = String
type InitialState  = [Constant]
type CommFunction  = Action -> Action -> Action

data LPPE = LPPE ProcessName ProcessPars [PSummand] deriving (Show, Eq)
	
data LPPEShowPRCRL = LPPEShowPRCRL PSpecification
instance Show LPPEShowPRCRL where
  show (LPPEShowPRCRL (LPPE name params summands, initialState, dataspec)) = datastring ++ "\n"  ++ name ++ printPairs (newParams) ++ 
                                                      " = \n      " ++ summandsString ++ "\n\ninit "++ name ++ "[" ++ (infixString initialState ", ") ++ "]"
    where
      summandsString = infixString (map (printPSummandPRCRL dataspec name params) summands) " \n    ++ "
      types                 = nub (["Bool"] 
                            ++ map (printType.snd) params ++ [printType t | summand <- summands, (v,t) <- getLocalPars summand]
                            ++ [printType t | summand <- summands, (v,t,f) <- getProbChoices summand] )
      functions      = concat (map getFunctionsInPSummand summands)
      dataspec2      = removeUnusedTypesAndFunctions dataspec (types \\ ["Bool"]) functions
      datastring     = "\n  \n" ++ printDataSpec dataspec2 ++ "\n  \n"
      newParams      = map (\x -> (fst x, printType (snd x))) params

data LPPEShow = LPPEShow PSpecification Bool
instance Show LPPEShow where
  show (LPPEShow (LPPE name params summands, initialState, dataspec) style) = name ++ printPairs newParams ++ " = \n      " ++ summandsString
                                                                              ++ "\n\nInitial state: " ++ name ++ commaList initialState
    where
      summandsString = infixString (map (printPSummand dataspec style name params) summands) " \n    + "
      newParams      = map (\x -> (fst x, printType (snd x))) params
      
type LocalPars   = [(Variable, Type)]
type Condition   = Expression
type ProbChoice  = (Variable, Type, ProbDistr)
type PSummand    = (LocalPars, Condition, Reward, Action, ActionPars, [ProbChoice], NextPars)

removeUnusedTypesAndFunctions :: DataSpec -> [String] -> [String] -> DataSpec
removeUnusedTypesAndFunctions dataspec types functions 
  = ([t | t <- fst3 dataspec, elem (getTypeName t) types], 
     [(TypeName t,v) | (TypeName t,v) <- snd3 dataspec, elem t types], 
     [FunctionDef f mappings | FunctionDef f mappings <- thd3 dataspec, elem f functions])

printType (TypeName name)     = name
printType (TypeRange from to) = "{" ++ show from ++ ".." ++ show to ++ "}"


------------------------------------
-- Functions for displaying LPPEs --
------------------------------------

-- This function prints a summand in pretty-print style.
printPSummand :: DataSpec -> Bool -> ProcessName -> ProcessPars -> PSummand -> String
printPSummand dataspec style procName procPars (params, c, reward, a, aps, probChoices, g) = 
  "(" ++ localParsToString ++ show c ++ " => " ++ a ++ commaList (map show aps)  
  ++ rewardString ++ " . " ++ printNextState dataspec style procName procPars (params, c, reward, a, aps, probChoices, g) probChoices g
  ++ (if (length params == 0) then "" else ")") ++ ")"
    where
      rewardString = if reward == Variable "0" then "" else "@(" ++ show reward ++ ")"
      parsList = (infixString [left ++ " : " ++ (printType right) | (left, right) <- params] ", ")
      localParsToString | length params == 0 = ""
	                    | otherwise          = "sum(" ++ parsList ++ ", "

-- This function prints a summand in prCRL style.
printPSummandPRCRL :: DataSpec -> ProcessName -> ProcessPars -> PSummand -> String
printPSummandPRCRL dataspec procName procPars (params, c, reward, a, aps, probChoices, g) = 
  "(" ++ localParsToString ++ printExpression c ++ " => " ++ a ++ commaList (map (printExpression) aps) ++ "@" ++ (printExpression reward)  
  ++ " . " ++ printNextState dataspec True procName procPars (params, c, reward, a, aps, probChoices, g) probChoices g
  ++ (if (length params == 0) then "" else ")") ++ ")"
    where
      parsList = (infixString [left ++ " : " ++ (printType right) | (left, right) <- params] ", ")
      localParsToString | length params == 0 = ""
	                    | otherwise          = "sum(" ++ parsList ++ ", "


-- This function prints the next state	
printNextState :: DataSpec -> Bool -> ProcessName -> ProcessPars -> PSummand -> [ProbChoice] -> NextPars -> String
printNextState dataspec style procName procPars summand probChoices g 
  | easier    = procName ++ printUpdates style procPars summand newNextState
  | otherwise = "psum(" ++ (probChoicesString) ++ " : " ++ procName ++ printUpdates style procPars summand g ++ ")"
  where
    easier            = and (map (== Variable "1") (map thd3 probChoices))
    substitutions     = [(v,Variable ((allValues t (fst3 dataspec))!!0)) | (v,t,Variable "1") <- probChoices]
    newNextState      = map (substituteInExpression substitutions) g
    probChoicesString = infixString (map (\x -> fst3 x ++ ": " ++ printType (snd3 x) ++ ", " ++ show (thd3 x)) probChoices) " || "

-- This function prints the updates to the state
printUpdates :: Bool -> ProcessPars -> PSummand -> NextPars -> String
printUpdates style procPars (params, c, reward, a, aps, probChoices, g) next = "[" ++ (infixString (getUpdates style params probChoices (zip (map fst procPars) next)) ", ") ++ "]"

-- This function produces the updates to the state
getUpdates style params probChoices [] = []
getUpdates True params probChoices ((parName, nextState):xs) = (show nextState):(getUpdates True params probChoices xs)
getUpdates False params probChoices ((parName, nextState):xs) | unchanged = getUpdates False params probChoices xs
                                                              | otherwise = (parName ++ " := " ++ show nextState):(getUpdates False params probChoices xs)
  where
	unchanged = nextState == Variable parName && (not (elem parName (map fst params)) ) && (not(elem parName (map fst3 probChoices)))

printInit :: [String] -> String
printInit x = "[" ++ infixString x "," ++ "]"

-----------------------------------------
-- General functions for dealing with  --
-- LPPEs, Specifications, and PSummands --
----------------------------------------

-- Given a specification, provides the LPPE
getLPPE :: PSpecification -> LPPE
getLPPE (lpe, initialState, dataspec) = lpe

-- Given a specification, provides the initial state
getInitialState :: PSpecification -> InitialState
getInitialState (lpe, initialState, dataspec) = initialState

-- Given a specification, provides the data part
getDataSpec :: PSpecification -> DataSpec
getDataSpec (lpe, initialState, dataspec) = dataspec

-- Provides the global parameters of an LPPE.
getLPPEPars :: LPPE -> ProcessPars
getLPPEPars (LPPE name pars summands) = pars

-- Looks up a summand of an LPPE.
getPSummand :: LPPE -> Int -> PSummand
getPSummand (LPPE name pars summands) i = summands!!i

-- Provideds all summands of an LPPE.
getPSummands :: LPPE -> [PSummand]
getPSummands (LPPE name pars summands) = summands

-- Provides a list of summand indices
getPSummandNrs :: LPPE -> [Int]
getPSummandNrs lppe = [0..length (getPSummands lppe) - 1]

-- Provides the action of a summand.
getAction :: PSummand -> Action
getAction (params, c, reward, a, aps, probChoices, g) = a

-- Provides the reward of a summand.
getReward :: PSummand -> Expression
getReward (params, c, reward, a, aps, probChoices, g) = reward

-- Changes the action of a summand.
setAction :: PSummand -> Action -> PSummand
setAction (params, c, reward, a, aps, probChoices, g) aNew = (params, c, reward, aNew, aps, probChoices, g)

-- Provides all actions of an LPPE
getActions :: LPPE -> [Action]
getActions lppe = map getAction (getPSummands lppe)

-- Provides the condition of a summand.
getCondition :: PSummand -> Expression
getCondition (params, c, reward, a, aps, probChoices, g) = c

-- Provides the action of a summand
getActionPars :: PSummand -> ActionPars
getActionPars (params, c, reward, a, aps, probChoices, g) = aps

-- Provides the local parameters of a summand
getLocalPars :: PSummand -> LocalPars
getLocalPars (params, c, reward, a, aps, probChoices, g) = params

-- Provides the probabilistic summations of a summand
getProbChoices :: PSummand -> [ProbChoice]
getProbChoices (params, c, reward, a, aps, probChoices, g) = probChoices

getNrParams :: PSpecification -> Int
getNrParams (LPPE name params newPSummands, initialState, dataSpec) = length params

-- Provides the next state for a certain parameter, given a summand.
getNextState :: PSummand -> Int -> Expression
getNextState (params, c, reward, a, aps, probChoices, g) i = g!!i

-- Provides the next state for all parameters, given a summand.
getNextStateAll :: PSummand -> [Expression]
getNextStateAll (params, c, reward, a, aps, probChoices, g) = g

makeConditionLPPE :: PSummand -> ProcessPars -> Expression
makeConditionLPPE summand pars = makeCondition pars (getNextStateAll summand)

-- This function takes a summand and adds a conjunct to its condition.
addImplication :: Expression -> PSummand -> PSummand
addImplication cNew (params, c, reward, a, aps, probChoices, g) = (params, Function "and" [c, cNew], reward, a, aps, probChoices, g)

addSummand :: LPPE -> PSummand -> LPPE
addSummand (LPPE name params summands) newSummand = LPPE name params (newSummand:summands)

-- This function takes an LPPE, an expression and a list of summands indices,
-- and adds the expression as a condition to every of these summands
addCondition :: LPPE -> Expression -> [Int] -> LPPE
addCondition (LPPE name pars summands) c nrs = LPPE name pars [if elem i nrs then addImplication c (summands!!i) else summands!!i | i <- [0..length summands - 1]]

-- This function takes a summand and adds a summation to its list of local parameters.
addSummation :: LocalPars -> PSummand -> PSummand
addSummation localPars (params, c, reward, a, aps, probChoices, g) = (params ++ localPars, c, reward, a, aps, probChoices, g)

-- This function substitutes values for parameters in an LPPE.
substituteInLPPE :: LPPE -> [(Int, String)] -> LPPE
substituteInLPPE (LPPE name pars summands) substitutions = LPPE name pars (map (substituteInPSummand subs) summands)
  where
    subs = [(fst (pars!!parNr), Variable value) | (parNr, value) <- substitutions]

-- This function substitutes values for parameters in a summand.
substituteInPSummand :: [Substitution] -> PSummand -> PSummand
substituteInPSummand subs (params, c, reward, a, aps, probChoices, g) = (params, newCondition, newReward, a, newAps, newProbChoices, newG)
  where
    subs2          = removeFromSubstitutionsList (map fst params) subs
    newCondition   = substituteInExpression subs2 c
    newReward      = substituteInExpression subs2 reward
    newAps         = map (substituteInExpression subs2) aps
    subs3          = removeFromSubstitutionsList (map fst3 probChoices) subs2
    newProbChoices = map (\x -> (fst3 x, snd3 x, substituteInExpression subs3 (thd3 x))) probChoices
    newG           = map (substituteInExpression subs3) g

getFunctionsInPSummand :: PSummand -> [String]
getFunctionsInPSummand (params, c, reward, a, aps, probChoices, g) = result
  where
    result = functionsInExpression c ++ functionsInExpression reward ++ concat (map functionsInExpression aps) ++ 
             concat (map functionsInExpression (map thd3 probChoices)) ++
             concat (map functionsInExpression g)

-- This function encapsulates (removes) actions in a system.
encapsulate :: PSystem -> [Action] -> PSystem
encapsulate system []                                        = system
encapsulate (LPPE name pars summands, init) (action:actions) = encapsulate newSystem actions
  where
    newSystem = (LPPE name pars [s | s <- summands, takeWhile (/= '{') (getAction s) /= action], init)

-- This function hides actions in a system.
hide :: PSystem -> [Action] -> PSystem
hide system []                                        = system
hide (LPPE name pars summands, init) (action:actions) = hide newSystem actions
  where
    newSystem = (LPPE name pars (   [(params, c, reward, "tau" ++ (dropWhile (/= '{') a), [], probChoices, g) | (params, c, reward, a, aps, probChoices, g) <- summands , takeWhile (\x -> x /= '{' && x /= '!' && x /= '?') a == takeWhile (\x -> x /= '{' && x /= '!' && x /= '?') action]
                                 ++ [s | s <- summands , takeWhile (\x -> x /= '{' && x /= '!' && x /= '?') (getAction s) /= takeWhile (\x -> x /= '{' && x /= '!' && x /= '?') action]), init)

-- This function renames actions in a system.
rename :: PSystem -> [(Action, Action)] -> PSystem
rename system []                                        = system
rename (LPPE name pars summands, init) ((actFrom, actTo):actions) = rename newSystem actions
  where
    newSystem = (LPPE name pars (   [(params, c, reward, actTo ++ dropWhile (/= '{') a, [], probChoices, g) | (params, c, reward, a, aps, probChoices, g) <- summands , takeWhile (/= '{') a == actFrom]
                                 ++ [s | s <- summands , takeWhile (/= '{') (getAction s) /= actFrom]), init)


-- This function removes (constant) parameters from an LPPE.
removeParametersFromLPPE :: PSpecification -> [Int] -> PSpecification
removeParametersFromLPPE ((LPPE name pars summands),initial,dataspec) indices = (LPPE name newPars newPSummands, newInitial, dataspec)
  where
    indicesToKeep = [i | i <- [0..length pars - 1], not(elem i indices)]
    newPars       = [pars!!i | i <- indicesToKeep]
    newPSummands   = map (removeParametersFromPSummand indicesToKeep) summands
    newInitial    = [initial!!i | i <- [0..length initial - 1], not(elem i indices)]

-- This function changes the next state function of a summand by omitting
-- some of the parameters. The parameter of this function indicates which
-- parameters should be kept.
removeParametersFromPSummand indicesToKeep (params, c, reward, a, aps, probChoices, g) = (params, c, reward, a, aps, probChoices, nextStatePars)
  where
    nextStatePars = [g!!i | i <- indicesToKeep]

-- This function adds a dummy parameter for dead variable reduction
addParameterToLPPE :: (LPPE, [String]) -> (LPPE, [String])
addParameterToLPPE ((LPPE name pars summands),initial) = (LPPE name newPars newPSummands, newInitial)
  where
    newPars       = pars++[("dummyVar_", TypeName "Bool")]
    newPSummands  = map addDummyToPSummand summands
    newInitial    = initial++["T"]

addDummyToPSummand (params, c, reward, a, aps, probChoices, g) = (params, Function "and" [c, Function "eq" [Variable "dummyVar_", Variable "T"]], reward, a, aps, probChoices, gNew)
  where
    gNew = g++[Variable "T"]

---------------------------------------
-- Functions for parallel composition --
---------------------------------------

-- This function suffixes all variables of an LPPE by a string.
suffixStringLPPE suffix (LPPE name pars summands) = LPPE name newPars newPSummands
  where
    newPars       = [(a ++ suffix,b)  | (a,b) <- pars] 
    substitutions = [(a, a ++ suffix) | (a,b) <- pars]
    newPSummands   = map (suffixStringPSummand suffix substitutions) summands

-- This function suffixes all variables of a summand by a string.
suffixStringPSummand suffix substitutions (params, c, reward, a, aps, [(v, t, f)], g) = (newParams, newC, newReward, newA, newAps, [(newV, t, newF)], newG)
--suffixStringPSummand suffix substitutions (params, c, a, aps, probChoices, g) = (newParams, newC, a, newAps, newProbChoices, newG)
   where
     newParams = [(a ++ suffix,b) | (a,b) <- params]
     newSubs   = nub (substitutions ++ [(a, a ++ suffix) | (a,b) <- params])
     newC      = substituteInExpression [(v,Variable e) | (v,e) <- newSubs] c
     newReward = substituteInExpression [(v,Variable e) | (v,e) <- newSubs] reward
     newAps    = map (substituteInExpression [(v,Variable e) | (v,e) <- newSubs]) aps
     newV      = v ++ suffix
     newSubs2  = nub (newSubs ++ [(v, newV)])
     newF      = substituteInExpression [(v,Variable e) | (v,e) <- newSubs2] f
     newG      = map (substituteInExpression [(v,Variable e) | (v,e) <- newSubs2]) g
     newA      = substituteInActionName [(v,Variable e) | (v,e) <- newSubs2] a     

suffixStringPSummand suffix substitutions (params, c, reward, a, aps, probChoices, g) = error("Initially, all summands should have one probabilistic choice.")

substituteInActionName substitutions action = aNew 
  where
      expression = (takeWhile (/= '}') (drop 1 (dropWhile (/= '{') action)))
      parsedExpressions_ = ParserExpressionsGlobals.parseExpression expression 1
      (Ok parsedExpressions)           = parsedExpressions_
      newExpressions = [(substituteInExpression substitutions f,substituteInExpression substitutions t) | (f,t) <- parsedExpressions]
      aNew_    = (takeWhile (/= '{') action) ++ "{" ++ printExpressions newExpressions ++ "}"	
      aNew     = if elem '{' action then aNew_ else action

--suffixStringProbChoices suffix newSubs [] = []
--suffixStringProbChoices suffix newSubs ((v,t,f):rest) = (newV, t, newF):(suffixStringProbChoices suffix newSubs rest)
--  where
--    newV      = v ++ suffix
--    newF      = substituteInExpression [(v,Variable e) | (v,e) <- newSubs2] f



-- This function takes two systems and a communication function,
-- and produces their parallel composition.
composeSystems :: Bool -> Bool -> [String] -> Bool -> PSystem -> PSystem -> [(Action,Action,Action)] -> (LPPE, [[Char]])
composeSystems sharedActions ioActions nocomm prism ((LPPE name1 pars1 summands1),initial1) ((LPPE name2 pars2 summands2),initial2) communications
  = (LPPE "Z" (pars1 ++ pars2) (combinePSummands sharedActions ioActions nocomm prism summands1 summands2 pars1 pars2 communications), initial1 ++ initial2)

-- This function takes two lists of summands of different processes,
-- and produces the corresponding summands in the parallel composition
-- of these processes.
combinePSummands :: Bool -> Bool -> [String] -> Bool -> [PSummand] -> [PSummand] -> ProcessPars -> ProcessPars -> [(Action,Action,Action)] -> [PSummand]
combinePSummands sharedActions ioActions nocomm prism summands1 summands2 pars1 pars2 communications = alone ++ together
  where
    shared   = [a | a <- getSharedActions summands1 summands2, not (elem a nocomm)]
    alone    = mergePSummands ioActions (if sharedActions || prism || ioActions then shared else []) summands1 summands2 pars1 pars2
    comm     = \x -> \y -> createCommFunction x y (communications ++ if sharedActions || ioActions || prism then [(a,a,a) | a <- shared, a /= "tau"] else [])
    together = communicatePSummands [(a,b) | a <- summands1, b <- summands2] comm

-- This function obtains the actions over which two systems (given by their summands)
-- can/must synchronise.
getSharedActions :: [PSummand] -> [PSummand] -> [Action]
getSharedActions summands1 summands2 = intersect (map removeExtensions (map getAction summands1)) (map removeExtensions (map getAction summands2))

removeExtensions action = takeWhile (/= '?') (takeWhile (/= '!') action)

isInternal action = takeWhile (/= '?') (takeWhile (/= '!') action) == action
isOutput action = (takeWhile (/= '!') action) ++ "!" == action
isInput action = (takeWhile (/= '?') action) ++ "?" == action


-- This function takes the union of the summands of two processes,
-- changing their next state function to take into account the new
-- parameters of the product process.
-- The actions provided to this functions are assumed to be shared actions,
-- and are mandatory to synchronise on.
mergePSummands :: Bool -> [Action] -> [PSummand] -> [PSummand] -> ProcessPars -> ProcessPars -> [PSummand]
mergePSummands io sharedActions [] [] pars1 pars2                                            
  = []
mergePSummands io sharedActions ((params, c, reward, a, aps, probChoices, g):summands1) summands2 pars1 pars2 
  | (io && (isInternal a)) || a == "tau" || not (elem (removeExtensions a) sharedActions) = [(params, c, reward, a, aps, probChoices, mergeNextStatesLeft g pars2)]  ++ (mergePSummands io sharedActions summands1 summands2 pars1 pars2)
  | otherwise                                = mergePSummands io sharedActions summands1 summands2 pars1 pars2
mergePSummands io sharedActions [] ((params, c, reward, a, aps, probChoices, g):summands2) pars1 pars2              
  | (io && (isInternal a)) || a == "tau" || not (elem (removeExtensions a) sharedActions) = [(params, c, reward, a, aps, probChoices, mergeNextStatesRight g pars1)]  ++ (mergePSummands io sharedActions [] summands2 pars1 pars2)
  | otherwise                                = mergePSummands io sharedActions [] summands2 pars1 pars2

-- This function changes the next state function to take
-- into account the new parameters (leaving them unchanged).
mergeNextStatesLeft  n pars2 = n ++ [Variable x | x <- (map fst pars2)]
mergeNextStatesRight n pars1 = [Variable x | x <- (map fst pars1)] ++ n

-- This function produces the summands that arise in parallel composition
-- by communication of actions.
communicatePSummands :: [(PSummand, PSummand)] -> CommFunction -> [PSummand]
communicatePSummands [] comm = []
communicatePSummands ((summand1, summand2):xs) comm 
  | communicable  = communicatePSummands xs comm ++ merge summand1 summand2 commAction2
  | otherwise        = communicatePSummands xs comm
  where
    a1 = takeWhile (/= '{') (getAction summand1)
    a2 = takeWhile (/= '{') (getAction summand2)
    commAction_  = comm (removeExtensions a1) (removeExtensions a2)
    commAction   = if commAction_ == "" then "" else
	                 if (isInput a1 && isInput a2) then commAction_ ++ "?" else 
	                 if (isInput a1 && isOutput a2) then commAction_ ++ "!" else
		             if (isOutput a1 && isInput a2) then commAction_ ++ "!" else
	        	     if (isOutput a1 && isOutput a2) then error("Outputs of parallel components not distinct: " ++ a1) else
		 		       commAction_
    communicable = commAction /= "" && length (getActionPars summand1) == length (getActionPars summand2)
    commAction2  = commAction ++ (combineGlobalUpdates (dropWhile (/= '{') (getAction summand1)) (dropWhile (/= '{') (getAction summand2))  )

combineGlobalUpdates first second | not(elem '{' first) && not(elem '{' second) = ""
                                  | not(elem '{' first)                         = second  
                                  | not(elem '{' second)                        = first
                                  | otherwise                                   = takeWhile (/= '}') first ++ ", " ++ (drop 1 (dropWhile (/= '{') second))

-- This function merges two summands that can communicate
-- over an action.
merge :: PSummand -> PSummand -> Action -> [PSummand]
merge (params1, c1, reward1, a1, aps1, probChoices1, g1) (params2, c2, reward2, a2, aps2, probChoices2, g2) action = result
     where
        apsEqual = [Function "eq" [a,b] | (a,b) <- zip aps1 aps2]
        newReward = if reward1 == Variable "0" && reward2 == Variable "0" then Variable "0" else Function "plus" [reward1, reward2]
        result   = [(params1 ++ params2, Function "and" ([c1, c2] ++ apsEqual), newReward, action, aps1, probChoices1 ++ probChoices2, g1 ++ g2)]

-- This function creates a communication function based on a list of 
-- (action, action,action) pairs. It checks whether the inputs x and y
-- can communicate, and if so, returns the corresponding communication action.
createCommFunction :: Action -> Action -> [(Action, Action, Action)] -> Action
createCommFunction x y []                          = ""
createCommFunction x y ((a,b,c):rest) | match      = c
                                      | otherwise  = createCommFunction x y rest 
  where
    match = (x == a && y == b) || (x == b && y == a)

-------------------------------------------------------------
-- Functions to check whether a process is already in LPPE --
-- form and to parse it indeed to LPPE in that case.       --
-------------------------------------------------------------

-- This function checks whether a process term is already
-- in LPPE format.
isLPPE :: String -> Int -> ProcessTerm -> Bool
isLPPE name nrPars (Plus summands) = and (map (checkPSummands name nrPars) summands)
isLPPE name nrPars term            = checkSummations name nrPars term

-- This functions checks whether a process term is a summand
-- by unfoldings the nondeterministic choices.
checkPSummands name nrPars (Plus summands) = and (map (checkPSummands name nrPars) summands)
checkPSummands name nrPars term            = checkSummations name nrPars term

-- This function walks through the summations of the summand.
checkSummations :: String -> Int -> ProcessTerm -> Bool
checkSummations name nrPars (Sum v t rhs)                                           = checkSummations name nrPars rhs
checkSummations name nrPars (Implication c rhs)                                     = checkConditions name nrPars rhs
checkSummations name nrPars (ActionPrefix reward a aps probs (ProcessInstance name2 pars)) = name == name2 && length pars == nrPars
checkSummations name nrPars other                                                   = False

-- This function walks through the conditions of the summand.
checkConditions :: String -> Int -> ProcessTerm -> Bool
checkConditions name nrPars (Implication c rhs) = checkConditions name nrPars rhs
checkConditions name nrPars (ActionPrefix reward a aps probs (ProcessInstance name2 pars)) = name == name2 && length pars == nrPars
checkConditions name nrPars other  = False

-- Given that a process term is in the right format, this function indeed
-- parses it to a list of summands.
parseLPPE :: ProcessTerm -> [PSummand]
parseLPPE (Plus [a,b]) = (parseLPPE a) ++ (parseLPPE b)
parseLPPE x            = [parsePSummand x [] (Variable "T")]

parsePSummand :: ProcessTerm -> ProcessPars -> Expression -> PSummand
parsePSummand (Sum v t rhs) pars c                                       = parsePSummand rhs (pars ++ [(v,t)]) c
parsePSummand (Implication cNew rhs) pars c | c == (Variable "T")        = parsePSummand rhs pars cNew
parsePSummand (Implication cNew rhs) pars c | otherwise                  = parsePSummand rhs pars (Function "and" [c, cNew])
parsePSummand (ActionPrefix reward a aps probs (ProcessInstance name p)) pars c = (pars, c, reward, a, aps, probs, p)
parsePSummand other pars c
  = error("Trying to interpret a specification as an LPPE even though it is not.")

----------------------
-- For PRISM parser --
----------------------

fixGlobalVariables :: PSystem -> PSystem
fixGlobalVariables ((LPPE name pars summands), initial) = (LPPE name newPars newPSummands, newInits)
  where
    (globals, newPars, newInits) = findGlobalVariables pars initial
    newPSummands        = map (fixGlobalsInPSummand globals pars) summands

findGlobalVariables :: ProcessPars -> InitialState -> ([String], ProcessPars, InitialState)
findGlobalVariables [] []                  = ([],[],[])
findGlobalVariables ((v,t):pars) (i:inits) = (global ++ restGlobals, (v,t):restPars, i:restInits)
  where
    newPars                            = [p | p <- pars, p /= (v,t)]
    indices                            = [i | i <- [0..length inits - 1], pars!!i /= (v,t)]
    newInits                           = [inits!!i | i <- indices]
    global                             = if newPars == pars then [] else [v]
    (restGlobals, restPars, restInits) = findGlobalVariables newPars newInits
 
fixGlobalsInPSummand :: [String] -> ProcessPars -> PSummand -> PSummand
fixGlobalsInPSummand [] pars (params, c, reward, a, aps, probChoices, g) = (params, c, reward, a, aps, probChoices, g)
fixGlobalsInPSummand (global:globals) pars (params, c, reward, a, aps, probChoices, g) | length newValue > 1 = error("Fault in fixGlobalsInPSummand")
                                                                              | otherwise           = result
  where
    indices      = [i | i <- [0..length pars - 1], fst (pars!!i) == global]
    firstIndex   = head indices
    newValue     = [g!!i | i <- indices, g!!i /= Variable global]
    realNewValue = if newValue == [] then Variable global else (newValue!!0)
    newG_        = [g!!i | i <- [0..length g - 1], not(elem i (tail indices))]
    newG         = [newG_!!i | i <- [0..firstIndex - 1]] ++ [realNewValue] ++ [newG_!!i | i <- [firstIndex + 1..length newG_ - 1]]
    result       = fixGlobalsInPSummand globals [pars!!i | i <- [0..length pars -1], not (elem i (tail indices))] (params, c, reward, a, aps, probChoices, newG)
