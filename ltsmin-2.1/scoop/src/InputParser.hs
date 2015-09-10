---------------------------------------------------------
-- Takes a string containing a prCRL specification and --
-- provides the system is describes.                   --
--                                                     --
-- Finished refactoring: 16-sep-2010                   --
-- Nog weer verbeteren! 
---------------------------------------------------------

module InputParser where

import LPPE
import Expressions
import Processes
import Linearise
import Auxiliary
import Parser
import ParserAux
import qualified ParserExpressions
import qualified ParserExpressionsGlobals
import DataSpec
import StepPA
import Data.List
import Debug.Trace
import MLPPE 

type Constants    = [(String, Expression)]
type Globals      = [(Variable, Type, Expression)]
type StateRewards = [(Expression, Expression)]

-----------------------
-- Parsing the input --
-----------------------

-- This function takes a prCRL specification, and returns the corresponding
-- LPPE including its initial state.
parseInput :: Bool -> Bool -> Bool -> Bool -> Bool -> Constants -> String -> (PSpecification, [(String, Type)], [String],[String], StateRewards)
parseInput isMA sharedActions ioActions mergeTransitions prismComposition constants1 input 
                                                              | not(isMA) && stateRewards /= [] = error("State rewards only allowed for use in combination with IMCA. Use the -ma flag to work with them.")   
                                                              | correctSpecification = ((fst system2, snd system2, newDataspec), actiontypes, untilformula, reachNew, stateRewards)
                                                              | otherwise            = error("Error in specification.")
  where
    input2                 = input -- removeLineBreaks input
    (processes, initials, nocomm, reach, reachCondition, stateRewards, hiding,encapsulation, renaming, communication, constants, datatypes, functions, actiontypes, untilformula, globals)  = parseProcesses constants1 dataspec input2 isMA
    dataspec               = (datatypes ++ builtInTypes, [], functions ++ builtInFunctions)
    system_                = fst (makeLPPE 1 (nrOfParallelProcesses initials > 1) sharedActions ioActions nocomm mergeTransitions prismComposition communication constants dataspec processes initials)
    system                 = if (prismComposition) then fixGlobalVariables system_ else system_
    system2a               = rename (hide (encapsulate system encapsulation) hiding) renaming
    system2_               = addGlobals system2a dataspec globals
    system2__              = if reachCondition == Variable "T" then system2_ else addReachCondition reachCondition system2_
    system2                = addStateRewards stateRewards system2__
    summands               = getPSummands (fst system2)
    reachNew               = if reachCondition == Variable "T" then reach else ("reachConditionAction":reach)
    pars                   = getLPPEPars (fst system2)
    sumTypes               = [t | summand <- summands, (v,t) <- getLocalPars summand] ++ [t | summand <- summands, (v,t,f) <- getProbChoices summand]
    types                  = nub ([TypeName "Bool"] 
                             ++ allTypes (fst3 dataspec)                            
                             ++ map snd pars ++ sumTypes)
    typeValues             = [(t, allValues t (fst3 dataspec)) | t <- types]
    newDataspec            = (fst3 dataspec, typeValues, thd3 dataspec)
    correctSpecification   = distinctProcesses processes &&
                             noUnguardedRecursion processes processes &&
                             validateData system2 newDataspec && 
                             correctInits system2 newDataspec &&
                             correctSumTypes sumTypes

nrOfParallelProcesses (InitSingleProcess _)      = 1
nrOfParallelProcesses (InitParallel proc1 proc2) = nrOfParallelProcesses proc1 + nrOfParallelProcesses proc2
nrOfParallelProcesses (InitHiding _ proc)        = nrOfParallelProcesses proc
nrOfParallelProcesses (InitEncapsulation _ proc) = nrOfParallelProcesses proc
nrOfParallelProcesses (InitRenaming _ proc)      = nrOfParallelProcesses proc

addReachCondition expression (lppe, initial) = (newLPPE, initial)
  where
    newLPPE = addSummand lppe ([], expression, Variable "0", "reachConditionAction", [], [], map (\x -> Variable (fst x)) (getLPPEPars lppe))

addStateRewards [] (lppe, initial)                             = (lppe, initial)
addStateRewards ((stateCondition, reward):rest) (lppe, initial) = (newLPPE, newInitial)
  where
    (newLPPE,newInitial) = addStateRewards rest ((addSummand lppe ([], stateCondition, Variable "0", "stateRewardAction", [reward], [], map (\x -> Variable (fst x)) (getLPPEPars lppe))), initial)
	
correctSumTypes [] = True
correctSumTypes (t:ts) | t == TypeName "Queue" = error ("Cannot sum over type Queue")
                       | t == TypeName "Nat" = error ("Cannot sum over type Nat")
				       | otherwise    = correctSumTypes ts

distinctProcesses :: [Process] -> Bool
distinctProcesses procs | correct      = True
                        | not(correct) = error("Error: Not all processes have a distinct name, namely: " ++ show erroneous)
  where
    correct   = nub (map getProcName procs) == map getProcName procs
    erroneous = (map getProcName procs) \\(nub (map getProcName procs))

noUnguardedRecursion :: [Process] -> [Process] -> Bool
noUnguardedRecursion allProcs []                          = True
noUnguardedRecursion allProcs (p:ps) | unguardedRecursion = error("Error: Unguarded recursion of process " ++ name)
                                     | otherwise          = noUnguardedRecursion allProcs ps
  where
    name               = getProcName p
    unguardedRecursion = elem name (reachableUnguarded allProcs [] (getRHS p))

reachableUnguarded :: [Process] -> [String] -> ProcessTerm -> [String]
reachableUnguarded allProcs seen (LambdaPrefix _ _ _)        = []
reachableUnguarded allProcs seen (ActionPrefix _ _ _ _ rhs)  = []
reachableUnguarded allProcs seen (Sum _ _ rhs)             = reachableUnguarded allProcs seen rhs
reachableUnguarded allProcs seen (Implication _ rhs)       = reachableUnguarded allProcs seen rhs
reachableUnguarded allProcs seen (ActionPrefix2 _ _ _ nexts) = []
reachableUnguarded allProcs seen (Plus rhss)               = concat (map (reachableUnguarded allProcs seen) rhss)
reachableUnguarded allProcs seen (ProcessInstance name _)    
  | elem name seen = []
  | otherwise      = name:(reachableUnguarded allProcs (name:seen) (getRHS (resolveProcess name allProcs))) 


validateData :: PSystem -> DataSpec -> Bool
validateData (lppe, init) (datatypes, _, functions) = valid
  where
    usedTypes = findTypesInLPPE lppe
    valid     = and (map (checkType datatypes) usedTypes)

correctInits :: PSystem -> DataSpec -> Bool
correctInits (lppe, init) dataspec | not enoughInitials = error("Error: Number of initial values not correct.")
                                   | result    = True
                                   | printType (snd (initials!!index)) == "Queue" = trace("Warning: Initial value potentially incorrect. Input " ++ fst (initials!!index) ++ " of type " ++ printType (snd (initials!!index)) ++ " has unexpected values.") True
                                   | otherwise = error("Error: Initial value incorrect. Input " ++ fst (initials!!index) ++ " is not of type " ++ printType (snd (initials!!index))) 
  where
    initials = zip init (map snd (getLPPEPars lppe))
    elements = [elem var (getValues dataspec typ) ||
                (typ == TypeName "Queue" && and [elem v (concat (map snd (snd3 dataspec))) || isInteger v | v <- split var ';']) || 
                (typ == TypeName "Nat" && isInteger var) | (var, typ) <- initials]
    enoughInitials = length init == length (getLPPEPars lppe) 
    result   = and elements 
    index    = [i | i <- [0..length elements - 1], elements!!i == False]!!0


checkType :: [DataType] -> Type -> Bool
checkType _ (TypeRange from to) = True
checkType _ (TypeName "Queue") = True
checkType _ (TypeName "Nat") = True
checkType [] (TypeName name) = error("Error: Undefined type " ++ name ++ " used.")
checkType ((EnumInt name from to):xs) (TypeName t) | t == name = True
                                        | otherwise = checkType xs (TypeName t)
checkType ((EnumString name values):xs) (TypeName t) | t == name = True
                                           | otherwise = checkType xs (TypeName t)
checkType a b = error("Error in function checkType: " ++ show a ++ ", " ++ show b) 

findTypesInLPPE (LPPE name params summands) = map snd params


parseProcesses :: Constants -> DataSpec -> String -> Bool -> ([Process], InitialProcessDefinition, [String], [String], Expression, StateRewards, [String], [String], [(String, String)], [(Action,Action,Action)], Constants, [DataType], [FunctionDef], [(String, Type)], [String], Globals)
parseProcesses commandlineConstants dataspec processesString isMA                  
    | not(correctParse)                           = error(errorMessage)
    | correctParse && correctInput && (isMA || paWithRates) = (processes5, initialProcesses, nocomm, reach, reachCondition, stateRewards, hiding, encapsulation, renaming, communications, constants, types, functions, actiontypes, untilformula, globals)
    | otherwise                                   = error(errorMessage)
  where
    parsedInput_                 = parse processesString 1
    correctParse                 = case parsedInput_ of Ok a -> True 
                                                        Failed s -> False
    (Failed errorMessage)        = parsedInput_
    (Ok parsedInput)             = parsedInput_ 
    processes1                   = getProcesses parsedInput
    initialProcessDefs           = getInitialProcesses parsedInput
    initialProcesses             = head initialProcessDefs
    datapart                     = parsedInput
    processes2                   = processes1 
    processes3                   = map (unfoldUpdates processes2) processes2 
    processes4                   = functionaliseContext 1 processes3
    functions                    = getFunctions datapartnew
    constantsBefore              = commandlineConstants ++ getConstants functions datapart
    constants                    = substituteConstantsInConstants constantsBefore
    processes5                   = map (substituteConstantsInProcess functions constants) processes4
    datapartnew                  = applyConstants datapart constants
    hiding                       = getHiddenActions datapartnew
    nocomm                       = "rate":(getNoCommActions datapartnew)
    reach                        = getReachActions datapartnew
    reachCondition               = substituteInExpression constants (getReachCondition datapartnew)
    stateRewards                 = map (\x -> (substituteInExpression constants (fst x), substituteInExpression constants (snd x))) (getStateRewards datapartnew)
    globals                      = getGlobals functions constants datapartnew
    renaming                     = getRenamedActions datapartnew
    encapsulation                = getEncapsulatedActions datapartnew
    communications               = getCommunications datapartnew
    types                        = getTypes functions datapartnew
    untilformula                 = getUntilFormula datapartnew
    actiontypes                  = getActionTypes functions datapartnew
    correctInput                 = checkInput initialProcessDefs
    paWithRates                  = and [checkRates (getRHS x) | x <- processes5]

checkInput initialProcesses | length initialProcesses == 0 = error("Error: no initial process defined.")
                            | length initialProcesses > 1  = error("Error: more than one initial process defined.")
                            | otherwise                    = True

checkRates (ProcessInstance _ _)    = True
checkRates (ProcessInstance2 _ _)   = True
checkRates (Sum _ _ rhs)            = checkRates rhs
checkRates (Plus rhss)              = and [checkRates rhs | rhs <- rhss]
checkRates (LambdaPrefix reward e rhs)     = error ("Error: PA with rate: " ++ show e ++ ". Either use the -ma option, or remove the rate.") 
checkRates (Implication _ rhs)      = checkRates rhs
checkRates (ActionPrefix _ _ _ _ rhs) = checkRates rhs
checkRates (ActionPrefix2 _ _ _ probdefs) = and [checkRates (snd pd) | pd <- probdefs]
	                  
applyConstants [] constants = []
applyConstants ((DataRangeType name from to):rest) constants = (DataRangeType name newFrom newTo):(applyConstants rest constants)
  where
    newFrom = substituteInExpression constants from
    newTo   = substituteInExpression constants to
applyConstants (x:rest) constants = x:(applyConstants rest constants)

getProcesses []                            = []
getProcesses ((ParserProcess proc):rest)   = proc:(getProcesses rest)
getProcesses (_:rest)                    = getProcesses rest

getInitialProcesses []           = []
getInitialProcesses ((ParserInitialProcess inits):rest) = inits:(getInitialProcesses rest)
getInitialProcesses (_:rest)                 = getInitialProcesses rest

getHiddenActions []                          = []
getHiddenActions ((DataHiding actions):rest) = actions ++ getHiddenActions rest
getHiddenActions (_:rest)                    = getHiddenActions rest

getNoCommActions []                                   = []
getNoCommActions ((DataNoCommunication actions):rest) = actions ++ getNoCommActions rest
getNoCommActions (_:rest)                             = getNoCommActions rest

getReachActions []                                   = []
getReachActions ((DataReach actions):rest) = actions ++ getReachActions rest
getReachActions (_:rest)                             = getReachActions rest

getReachCondition []                                   = Variable "T"
getReachCondition ((DataReachCondition e):rest) = Function "and" [e, getReachCondition rest]
getReachCondition (_:rest)                             = getReachCondition rest

getStateRewards []                                   = []
getStateRewards ((DataStateReward state reward):rest) = ((state, reward)):(getStateRewards rest)
getStateRewards (_:rest)                             = getStateRewards rest

getGlobals _ _ []                                         = []
getGlobals functions constants ((DataGlobal (name, typ) value):rest) = (name, substituteConstantsInType functions constants typ, Variable (evalExpr functions [] value)):(getGlobals functions constants rest)
getGlobals functions constants (_:rest)                           = getGlobals functions constants rest

getRenamedActions []                                = []
getRenamedActions ((DataRenaming actionpairs):rest) = actionpairs ++ getRenamedActions rest
getRenamedActions (_:rest)                          = getRenamedActions rest

getUntilFormula []                                = []
getUntilFormula ((DataUntilFormula formula):rest) = formula:(getUntilFormula rest)
getUntilFormula (_:rest)                          = getUntilFormula rest

getEncapsulatedActions []                                 = []
getEncapsulatedActions ((DataEncapsulation actions):rest) = actions ++ getEncapsulatedActions rest
getEncapsulatedActions (_:rest)                           = getEncapsulatedActions rest

getCommunications []                               = []
getCommunications ((DataCommunication comms):rest) = comms ++ getCommunications rest
getCommunications (_:rest)                         = getCommunications rest

getConstants _ []                               = []
getConstants functions ((DataConstant name value):rest) = (name, value):(getConstants functions rest)
getConstants functions (_:rest)                         = getConstants functions rest
-- Dit veranderd; nu hoeven constanten niet altijd geevalueerd te worden.
--getConstants functions ((DataConstant name value):rest) = (name, Variable (evalExpr ([], [], functions) [] value)):(getConstants functions rest)

getActionTypes :: [FunctionDef] -> [Item] -> [(Action, Type)]
getActionTypes functions []                           = []
getActionTypes functions ((DataActions actions):rest) = map (evaluateActionType functions) actions ++ (getActionTypes functions rest)
getActionTypes functions (_:rest)                     = getActionTypes functions rest

evaluateActionType functions (name, None)                       = (name, NoType)
evaluateActionType functions (name, Bool)                       = (name, TypeName "Bool")
evaluateActionType functions (name, IntRange from to) | correct = (name, TypeRange (read fromInt) (read toInt))
                                                      | otherwise = error("Error: could not parse the action type " ++ name ++ " = {" ++ show from ++ ".." ++ show to ++ "}")
  where
    fromInt = evalExpr functions [] from
    toInt   = evalExpr functions [] to
    correct = isInteger fromInt && isInteger toInt


getTypes functions []                                                   = []
--getTypes functions ((DataNat name):rest)                                = (NatType name):(getTypes functions rest)
--getTypes functions ((DataQueue name):rest)                              = (QueueType name):(getTypes functions rest)
getTypes functions ((DataEnumType name values):rest)   | name == "Bool" || name == "Queue" || name == "Nat" = error ("Cannot redefine type " ++ name)
                                                       | otherwise                                          = (EnumString name values):(getTypes functions rest)
getTypes functions ((DataRangeType name from to):rest) | name == "Bool" || name == "Queue" || name == "Nat" = error ("Cannot redefine type " ++ name)
                                                       | correct = (EnumInt name (read fromValue) (read toValue)):(getTypes functions rest)
                                                       | otherwise = error("Error: could not parse the type " ++ name ++ " = {" ++ show from ++ ".." ++ show to ++ "}")
  where
    fromValue = evalExpr functions [] from
    toValue   = evalExpr functions [] to
    correct   = isInteger fromValue && isInteger toValue 
getTypes functions (_:rest)                                             = getTypes functions rest

getFunctions []                                 = []
getFunctions ((DataFunction name mapping):rest) = (FunctionDef name mapping):(getFunctions rest)
getFunctions (_:rest)                           = getFunctions rest


functionaliseContext i [] = []
functionaliseContext i (r:rs) = (functionaliseProbabilities i r):(functionaliseContext (i+1) rs)

makeLPPE :: Int -> Bool -> Bool -> Bool -> [String] -> Bool -> Bool -> [(Action,Action,Action)] -> Constants -> DataSpec -> [Process] -> InitialProcessDefinition -> (PSystem, Int)
makeLPPE nr addIndices sharedActions ioActions nocomm mergeTransitions prismComposition communication constants dataspec processes (InitSingleProcess proc)   = (system, nr + 1)
  where
    (lppe, init) = processToLPPE mergeTransitions constants dataspec processes proc
    system = (if addIndices then suffixStringLPPE ("_" ++ show nr) lppe else lppe, init)
makeLPPE nr addIndices sharedActions ioActions nocomm mergeTransitions prismComposition communication constants dataspec processes (InitParallel proc1 proc2) = (composeSystems sharedActions ioActions nocomm prismComposition system1 system2 communication, nr2)
  where
    (system1,nr1) = makeLPPE nr addIndices sharedActions ioActions nocomm mergeTransitions prismComposition communication constants dataspec processes proc1
    (system2, nr2) = makeLPPE nr1 addIndices sharedActions ioActions nocomm mergeTransitions prismComposition communication constants dataspec processes proc2
makeLPPE nr addIndices sharedActions ioActions nocomm mergeTransitions prismComposition communication constants dataspec processes (InitHiding actions proc) = (hide system actions, nr2)
  where
    (system,nr2) = makeLPPE nr addIndices sharedActions ioActions nocomm mergeTransitions prismComposition communication constants dataspec processes proc	
makeLPPE nr addIndices sharedActions ioActions nocomm mergeTransitions prismComposition communication constants dataspec processes (InitEncapsulation actions proc) = (encapsulate system actions, nr2)
  where
    (system,nr2) = makeLPPE nr addIndices sharedActions ioActions nocomm mergeTransitions prismComposition communication constants dataspec processes proc	
makeLPPE nr addIndices sharedActions ioActions nocomm mergeTransitions prismComposition communication constants dataspec processes (InitRenaming actions proc) = (rename system actions, nr2)
  where
    (system,nr2) = makeLPPE nr addIndices sharedActions ioActions nocomm mergeTransitions prismComposition communication constants dataspec processes proc	

processToLPPE :: Bool -> Constants -> DataSpec -> [Process] -> InitialProcess -> PSystem
processToLPPE mergeTransitions constants dataspec processesMA (InitialProcess name initials constantdefs) = system
  where
    processes                   = map (substituteConstantsInProcess (thd3 dataspec) [(s1, Variable s2) | (s1, s2) <- constantdefs]) (map encodeMA processesMA)
    Process procname params rhs = resolveProcess name processes
    newProcesses                = (Process procname params rhs):[p | p <- processes, getProcName p /= name]
    initialProcess              = [i | i <- [0..length processes - 1], getProcName (processes!!i) == name]!!0
    newInitials                 =  map (\x -> if   elem x (map fst constants) 
                                              then ([val | (y, Variable val) <- constants, x == y]!!0)
   	                                          else x) (map (evalExpr (thd3 dataspec) []) initials)
    system                      = if (isLPPE procname (length params) rhs)    
	                              then (LPPE procname params (parseLPPE rhs), newInitials) 
	                              else (linearise dataspec newProcesses newInitials)

encodeMA :: Process -> Process
encodeMA (Process name pars rhs) = Process name pars (encodeProcessTerm rhs)

encodeProcessTerm (Plus rs)                            = Plus (map encodeProcessTerm rs)
encodeProcessTerm (Implication c rhs)                  = Implication c (encodeProcessTerm rhs)
encodeProcessTerm (LambdaPrefix (Variable "0") l rhs)  = ActionPrefix (Variable "0") "rate" [l] [("i0", TypeRange 1 1, (Variable "1"))] (encodeProcessTerm rhs)
encodeProcessTerm (LambdaPrefix reward l rhs)          = encodeProcessTerm (LambdaPrefix (Variable "0") l (ActionPrefix reward "tau" [] [("i0", TypeRange 1 1, (Variable "1"))] rhs))
encodeProcessTerm (Sum var typ rhs)                    = Sum var typ (encodeProcessTerm rhs)
encodeProcessTerm (ActionPrefix reward a aps probs rh) = ActionPrefix reward a aps probs (encodeProcessTerm rh)
encodeProcessTerm (ProcessInstance name pars)          = ProcessInstance name pars

substituteConstantsInConstants :: Constants -> Constants
substituteConstantsInConstants [] = []
substituteConstantsInConstants ((name,expr):rest) = (name,expr):(substituteConstantsInConstants newRest)
  where
    newRest = [(n,substituteInExpression [(name,expr)] e) | (n,e) <- rest]

substituteConstantsInProcess :: [FunctionDef] -> Constants -> Process -> Process
substituteConstantsInProcess functions subs (Process name params rhs) = newProcess
  where
    newRHSs    = substituteConstantsInProcessTerm functions subs rhs
    newParams  = [(v,substituteConstantsInType functions subs t) | (v,t) <- params]
    newProcess = Process name newParams newRHSs

substituteConstantsInProcessTerm :: [FunctionDef] -> [(Variable, Expression)] -> ProcessTerm -> ProcessTerm
substituteConstantsInProcessTerm functions subs (Plus rs)           = Plus (map (substituteConstantsInProcessTerm functions subs) rs)
substituteConstantsInProcessTerm functions subs (Implication c rhs) = Implication (substituteInExpression subs c) 
                                                                        (substituteConstantsInProcessTerm functions subs rhs)
substituteConstantsInProcessTerm functions subs (LambdaPrefix reward l rhs) = LambdaPrefix (substituteInExpression subs reward) (substituteInExpression subs l) 
                                                                        (substituteConstantsInProcessTerm functions subs rhs)
substituteConstantsInProcessTerm functions subs (Sum var typ rhs)   = Sum var typNew (substituteConstantsInProcessTerm functions subs rhs)
  where
    typNew = substituteConstantsInType functions subs typ
substituteConstantsInProcessTerm functions subs (ActionPrefix reward a aps probs rhs) = ActionPrefix newReward aNew newAps newProbs (substituteConstantsInProcessTerm functions subs rhs)
    where
      expression = (takeWhile (/= '}') (drop 1 (dropWhile (/= '{') a)))
      parsedExpressions_ = ParserExpressionsGlobals.parseExpression expression 1
      (Ok parsedExpressions)           = parsedExpressions_
      newExpressions = [(substituteInExpression subs f,substituteInExpression subs t) | (f,t) <- parsedExpressions]
      aNew_    = (takeWhile (/= '{') a) ++ "{" ++ printExpressions newExpressions ++ "}"	
      aNew     = if elem '{' a then aNew_ else a
      newReward = substituteInExpression subs reward
      newAps   = map (substituteInExpression subs) aps
      newProbs = [(v, substituteConstantsInType functions subs t, substituteInExpression subs f) | (v,t,f) <- probs]
substituteConstantsInProcessTerm functions subs (ProcessInstance name pars) = ProcessInstance name (map (substituteInExpression subs) pars)

substituteConstantsInType :: [FunctionDef] -> [Substitution] -> Type -> Type
substituteConstantsInType functions subs (TypeName name)                = TypeName name
substituteConstantsInType functions subs (TypeRange from to)            = TypeRange from to
substituteConstantsInType functions subs (TypeRangeExpressions from to) | correct   = TypeRange (read newFrom) (read newTo)
                                                                        | otherwise = error("Error: could not parse type {" ++ show from ++ ".." ++ show to ++ "}")
  where
    newFrom = evalExpr functions [] (substituteInExpression subs from) 
    newTo   = evalExpr functions [] (substituteInExpression subs to)
    correct = isInteger newFrom && isInteger newTo

----------------------
-- Global variables --
----------------------

addGlobals :: PSystem -> DataSpec -> Globals -> PSystem	
addGlobals (LPPE name params summands, initial) _ []                          = (LPPE name params (map removeGlobalSuffixes summands), initial)
addGlobals (LPPE name params summands, initial) dataspec ((var,typ,value):gs) = addGlobals (LPPE name (params ++ [(var,typ)]) newSummands, initial ++ [globalInit]) dataspec gs
  where
    globalInit  = evalExpr (thd3 dataspec) [] value
    newSummands = map (addGlobalToNextStates var) summands

addGlobalToNextStates :: Variable -> PSummand -> PSummand
addGlobalToNextStates var (params, c, reward, a, aps, prob, g) | a == "setGlobal" && length aps == 2 && (getGlobalName (aps!!0) == var)= (params, c, reward, "tau", [], prob, g ++ [aps!!1])
                                                       | elem '{' a && elem var (map fst parsedExpressions)            = if (length (nub newValues) > 1) then error("Assigned a new value for global variable " ++ var ++ " more than once at the same time (maybe due to communicating actions): " ++ show newValues) else (params, c, reward, a, aps, prob, g ++ [newValues!!0])
                                                       | otherwise                                                     = (params, c, reward, a, aps, prob, g ++ [Variable var])
  where
    expression = (takeWhile (/= '}') (drop 1 (dropWhile (/= '{') a)))
    parsedExpressions_ = ParserExpressionsGlobals.parseExpression expression 1
    (Ok parsedExpressions__)           = parsedExpressions_
    parsedExpressions                                        = [(v,val) | (Variable v,val) <- parsedExpressions__] ++ 
                                                               [(v1++v2,val) | (Function "concat" [Variable v1, Variable v2],val) <- parsedExpressions__]
    newValues = [val | (v,val) <- parsedExpressions, v == var]

getGlobalName (Variable var) = var
getGlobalName (Function "concat" [Variable v1,Variable v2]) = v1 ++ v2
getGlobalName other                                             = error("Could not parse global update " ++ show other)

removeGlobalSuffixes (params, c, reward, a, aps, prob, g) = (params, c, reward, takeWhile (/= '{') a, aps, prob, g)
