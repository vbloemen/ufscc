------------------
--- CONFLUENCE ---
------------------

module Confluence where

import DeadVariable
import Auxiliary
import Simplify
import Expressions
import LPPE
import Data.List
import DataSpec
import Usage
import Debug.Trace

type ConfluentSummands = [Int]

-- This function takes a specification and a boolean, and returns a list
-- of indices that point at confluent summands.
-- If the boolean is true, strong (more time-consuming) detection heuristics
-- are applied, otherwise only weak heuristics are used.
getConfluentSummands :: PSpecification -> Bool -> [String] -> [Int]
getConfluentSummands (lppe, init, dataspec) strong reachActions = result
  where
    summands         = getPSummands lppe
    pars             = getLPPEPars lppe
    summandNrs       = getPSummandNrs lppe
    usageInSummands  = [(i, getUsedInSummand lppe i) | i <- summandNrs]
    restrictedUsageInCondition  = [(i, getUsedOnlyInGreaterThanCondition lppe i) | i <- summandNrs]
    changedUnchanged = [(i, getChangedUnchanged lppe i) | i <- summandNrs]  -- changed to the same value that was required in the condition
    restrictedChanged           = [(i, getRestrictedChanged lppe i)              | i <- summandNrs]
    mapping          = zip (map fst pars) (map snd pars)
    summandsNumbered = [(i, summands!!i) | i <- summandNrs]
    result           = [i | i <- summandNrs, summandIsConfluent dataspec strong usageInSummands restrictedUsageInCondition restrictedChanged changedUnchanged (summands!!i) i summandsNumbered mapping lppe reachActions]

-- This function checks whether a specific summand is confluent. For this,
-- it requires the data specification, a boolean to signale whether or not
-- to apply strong heuristics, the usage of variables in the summands, the
-- summand that needs to be checked, its index, a list of all summands
-- (together with their indices), a mapping of the global variables to types,
-- and the LPPE in which the summand is contained.
summandIsConfluent :: DataSpec -> Bool -> [(Int, [Int])] -> [(Int, [Int])] -> [(Int, [Int])] -> [(Int, [Int])] -> PSummand -> Int -> [(Int, PSummand)] -> TypeMapping -> LPPE -> [String] -> Bool
summandIsConfluent dataspec strong usageInSummands restrictedUsage restrictedChanged changedUnchanged summand summandNr summands mapping lppe reachActions = istau && uniqueTarget && isConfluent
  where
    istau        = getAction summand == "tau" && getActionPars summand == [] && getReward summand == Variable "0"
    uniqueTarget = and (map (\x -> length (getValues dataspec x) == 1) (map snd3 (getProbChoices summand)))
    confluence   = map (confluentPair dataspec strong usageInSummands restrictedUsage restrictedChanged changedUnchanged lppe mapping (summandNr, summand) reachActions) summands
    isConfluent  = and confluence

-- This function checks whether a pair of summands commutes. It should be provided
-- with the data specification, a bool to signal whether or not to use strong heuristics,
-- a list of global variable indices that are used in each summand, and lppe, a type mapping,
-- and the two summands.
-- It checks whether the summands are either never enabled at the same time,
-- or are completely independent, or are the same and both yield a single confluent transition.
confluentPair :: DataSpec -> Bool -> [(Int, [Int])] -> [(Int, [Int])] -> [(Int, [Int])] -> [(Int, [Int])] -> LPPE -> TypeMapping -> (Int, PSummand) -> [String] -> (Int, PSummand) -> Bool
confluentPair dataspec strong usageInSummands restrictedUsage restrictedChanged changedUnchanged lppe mapping (i, (params1, c1, reward1, a1, aps1, probChoices1, g1)) reachActions
 	                                                                           (j, (params2, c2, reward2, a2, aps2, probChoices2, g2)) = result
  where
    pars            = getLPPEPars lppe
    disjoint        = if (strong) then neverBothEnabledPairs dataspec c1 c2 (mapping ++ params1 ++ params2) (mapping ++ params1 ++ params2) 
	                                || neverBothEnabled dataspec c1 c2 [(v,t,getValues dataspec t) | (v,t) <- (mapping ++ params1 ++ params2)]
	                  else neverBothEnabled dataspec c1 c2 [(v,t,getValues dataspec t) | (v,t) <- (mapping ++ params1 ++ params2)] 
    changedUnchangedInFirst = [used | (sumNr,used) <- changedUnchanged, sumNr == i]!!0
    changedUnchangedInSecond = [used | (sumNr,used) <- changedUnchanged, sumNr == j	]!!0
    changedInFirst  = [v | v <- getChangedInNextStateNums (params1, c1, reward1, a1, aps1, probChoices1, g1) (zip [0..length(pars) - 1] (map fst pars)), not (elem v changedUnchangedInFirst)]
    changedInSecond = [v | v <- getChangedInNextStateNums (params2, c2, reward1, a2, aps2, probChoices2, g2) (zip [0..length(pars) - 1] (map fst pars)), not (elem v changedUnchangedInSecond)]
    usedInFirst     = [used | (sumNr,used) <- usageInSummands, sumNr == i]!!0
    usedInSecond    = [used | (sumNr,used) <- usageInSummands, sumNr == j]!!0
    usedInConditionFirst  = getUsedInCondition lppe i
    usedInConditionSecond = getUsedInCondition lppe j
    usedInActionFirst  = getUsedInAction lppe i
    usedInActionSecond = getUsedInAction lppe j
    usedInRewardFirst  = getUsedInReward lppe i
    usedInRewardSecond = getUsedInReward lppe j
    usedInProbsFirst  = getUsedInProbs lppe i
    usedInProbsSecond = getUsedInProbs lppe j
    usedInNextFirst = getUsedInNext lppe i
    usedInNextSecond = getUsedInNext lppe j
    changedInNextFirstLinear = getChangedLinear lppe i
    changedInNextSecondLinear = getChangedLinear lppe j
    restrictedUsedInFirst     = [used | (sumNr,used) <- restrictedUsage, sumNr == i]!!0 
    restrictedUsedInSecond    = [used | (sumNr,used) <- restrictedUsage, sumNr == j]!!0
    restrictedChangedInFirst  = [used | (sumNr,used) <- restrictedChanged, sumNr == i]!!0 
    restrictedChangedInSecond = [used | (sumNr,used) <- restrictedChanged, sumNr == j]!!0
    independent     = (changedInFirst \\ usedInSecond == changedInFirst) && (changedInSecond \\ usedInFirst == changedInSecond)
    uniqueTarget    = and (map (\x -> length (getValues dataspec x) == 1) (map snd3 probChoices1))
    samesummand     = i == j && a1 == "tau" && params1 == [] && uniqueTarget
    independent2    = and [not (elem v usedInConditionSecond) || (elem v restrictedUsedInSecond && elem v restrictedChangedInFirst) | v <- changedInFirst] &&
                      and [not (elem v usedInConditionFirst)  || (elem v restrictedUsedInFirst && elem v restrictedChangedInSecond) | v <- changedInSecond] && 
                      (changedInFirst  \\ (usedInActionSecond ++ usedInProbsSecond ++ usedInRewardSecond) == changedInFirst) && 
                      (changedInSecond \\ (usedInActionFirst  ++ usedInProbsFirst ++ usedInRewardFirst)  == changedInSecond) &&
                      and [not (elem v usedInNextSecond) || (elem v changedInNextFirstLinear && elem v changedInNextSecondLinear) | v <- changedInFirst] &&
                      and [not (elem v usedInNextFirst) || (elem v changedInNextFirstLinear && elem v changedInNextSecondLinear) | v <- changedInSecond]

    -- Check if the confluent transition cannot enable a reachAction
    invisible       = (not (elem a2 [takeWhile (/= '(') r | r <- reachActions])) ||     -- it's not a reach action, so nothing to check 
                      (and [not (elem v usedInConditionSecond) -- || (elem v restrictedUsedInSecond && elem v restrictedChangedInFirst) 
                          --  || (elem v restrictedUsedInSecond && not(elem ((map fst pars)!!v) (map fst params1)) && not (elem ((map fst pars)!!v) (map fst3 probChoices1))
						   --        && ((g1!!v) == Variable ((map fst pars)!!v) || isSubtraction (g1!!v) ((map fst pars)!!v)))
                            || changeDisables (g1!!v) ((map fst pars)!!v) c2 | v <- changedInFirst])
                     -- above: the confluent summand does not influence the reachAction's condition 
    otherIsRate     = take 4 a2 == "rate"

    result          = otherIsRate || ((disjoint || independent || samesummand || independent2) && invisible)

changeDisables (Variable i) var c = isInteger i && disabling
  where
    values                 = possibleValuesForConditionToBeTrue var c
    PossibleValues values2 = values  
    disabling              = values /= Undecided && not (elem (Variable i) values2) 
changeDisables _ _ _       = False

-- This function checks whether two conditions are mutually exclusive.
-- For that, it ranges over all global variables, and for each it checks
-- whether it has as least one value for which both c1 and c2 are enabled.
-- If no such value exists (for at least one of the global variables), 
-- the conditions are mutually exclusive.
neverBothEnabled :: DataSpec -> Expression -> Expression -> [(String,Type,[String])] -> Bool
neverBothEnabled dataspec c1 c2 []                      = False
neverBothEnabled dataspec c1 c2 ((v,t,values):mappings) = disjoint
  where
    disjoint = (t /= TypeName "Queue" && t /= TypeName "Nat" && (neverBothEnabled2 dataspec c1 c2 v values)) || (neverBothEnabled dataspec c1 c2 mappings)	  

-- This function checks for a global variable whether it does not have
-- at least one value for which both c1 and c2 are enabled.
neverBothEnabled2 dataspec c1 c2 var []           = True
neverBothEnabled2 dataspec c1 c2 var (val:values) = result
   where
      notc1  = simplifyExpression dataspec (substituteInExpression [(var, (Variable val))] c1) == Variable "F"
      notc2  = simplifyExpression dataspec (substituteInExpression [(var, (Variable val))] c2) == Variable "F"
      result = (notc1 || notc2) && neverBothEnabled2 dataspec c1 c2 var values

-- If strong heuristics should be applied, this function checks a similar property
-- as neverBothEnabled, but now it also looks at all combinations of values for
-- pairs of global variables. This finds mutual exclusion in for instance the case
-- that c1 = a > b and c2 = a <= b.
neverBothEnabledPairs :: DataSpec -> Expression -> Expression -> TypeMapping -> TypeMapping -> Bool
neverBothEnabledPairs dataspec c1 c2 [] []                                    = False
neverBothEnabledPairs dataspec c1 c2 ((v,t):mappings) []                      =  neverBothEnabledPairs dataspec c1 c2 mappings mappings
neverBothEnabledPairs dataspec c1 c2 ((v,t):mappings) ((v2,t2):mappings2) 
  | v /= v2 && (variablesInFunction v v2 c1) || (variablesInFunction v v2 c2) = disjoint
  | otherwise                                                                 = neverBothEnabledPairs dataspec c1 c2 ((v,t):mappings) mappings2
  where
    disjoint = (t /= TypeName "Queue" && t2 /= TypeName "Queue" && t /= TypeName "Nat" && t2 /= TypeName "Nat" && neverBothEnabledPairs2 dataspec c1 c2 v (getValues dataspec t) v2 (getValues dataspec t2)) || (neverBothEnabledPairs dataspec c1 c2 ((v,t):mappings) mappings2)

-- This function checks for a pair of global variables whether they does not have
-- at least one value for which both c1 and c2 are enabled.
neverBothEnabledPairs2 dataspec c1 c2 var [] _ _                    = True
neverBothEnabledPairs2 dataspec c1 c2 var (val:values) var2 values2 = result
  where
    newc1  = simplifyExpression dataspec (substituteInExpression [(var, (Variable val))] c1)
    newc2  = simplifyExpression dataspec (substituteInExpression [(var, (Variable val))] c2)
    result = neverBothEnabled2 dataspec newc1 newc2 var2 values2 && neverBothEnabledPairs2 dataspec c1 c2 var values var2 values2

doSyntacticConfluence :: PSpecification -> ConfluentSummands -> PSpecification
doSyntacticConfluence spec conf | detectConfluentCycle spec conf = error(message)
                                | otherwise                      = syntacticConfluence spec conf conf
  where
    message = "Error: could not use syntactic confluence reduction for PRISM output,\nas there might be a cycle of confluent transitions.\n"
             ++"Please hide less actions, or include the -removecycles flag."

syntacticConfluence :: PSpecification -> ConfluentSummands -> [Int] -> PSpecification
syntacticConfluence spec confSums []     = spec
syntacticConfluence (lppe, init, dataspec) confSums (i:is) = syntacticConfluence ((addCondition lppe (Function "not" [c]) ((getPSummandNrs lppe \\ confSums) ++ [j | j <- confSums, j < i])), init, dataspec) confSums is
  where
    c = getCondition ((getPSummands lppe)!!i)

removeConfluentCycles :: PSpecification -> ConfluentSummands -> ConfluentSummands
removeConfluentCycles (lppe, initial, dataspec) confluents | or onCycle == False = confluents
                                                           | otherwise           = removeConfluentCycles (lppe, initial, dataspec) newConfluents
  where
    changed       = getChanged lppe
    rules         = getRules2 dataspec lppe
    cfps          = getCFPs dataspec lppe changed rules
    onCycle       = [summandOnConfluentCycle (lppe, initial, dataspec) changed confluents i cfps | i <- confluents]
    remove        = (elemIndices True onCycle)!!0
    newConfluents = [confluents!!i | i <- [0.. length confluents -1], i /= remove]

detectConfluentCycle :: PSpecification -> ConfluentSummands -> Bool
detectConfluentCycle (lppe, initial, dataspec) confluents = or [summandOnConfluentCycle (lppe, initial, dataspec) changed confluents i cfps | i <- confluents]
  where
    changed       = getChanged lppe
    rules         = getRules2 dataspec lppe
    cfps          = getCFPs dataspec lppe changed rules

summandOnConfluentCycle :: PSpecification -> Changed -> ConfluentSummands -> Int -> [Int] -> Bool
summandOnConfluentCycle spec changed confluents confNr cfps = not (or (map (cfpRulesOutConfluentCycle spec changed confluents confNr) cfps))
                                                           && not (or [cfpPairRulesOutConfluentCycle spec changed confluents confNr c1 c2 | c1 <- cfps, c2 <- cfps, c1 /= c2])

cfpRulesOutConfluentCycle :: PSpecification -> Changed -> ConfluentSummands -> Int -> Int -> Bool
cfpRulesOutConfluentCycle (lppe,initial,dataspec) changed confluents confNr cfp |  before == Bottom 
                                                                        || after  == Bottom = False
                                                                        | otherwise         = result
  where
    before              = source dataspec lppe confNr cfp 
    (Value beforeValue) = before
    after               = destination dataspec lppe confNr cfp
    (Value afterValue)  = after   
    possibleValues      = getPossibleValuesOfCFP lppe dataspec changed (confluents \\ [confNr]) cfp [afterValue]
    result              = not (elem beforeValue possibleValues)

getPossibleValuesOfCFP :: LPPE -> DataSpec -> Changed -> ConfluentSummands -> Int -> [String] -> [String]
getPossibleValuesOfCFP lppe dataspec changed confluents cfp values | newValues == values = values
                                                                   | otherwise           = getPossibleValuesOfCFP lppe dataspec changed confluents cfp newValues
  where
    newValues     = nub (values ++ [afterValue dataspec changed lppe i cfp v | i <- confluents, v <- values, enabled dataspec lppe i [(cfp, v)]])

enabled :: DataSpec -> LPPE -> Int -> [(Int, String)] -> Bool
enabled dataspec lppe summand subs = result
  where
    LPPE name pars sums = lppe
    subs2               = [(fst (pars!!parIndex), Variable value) | (parIndex, value) <- subs]
    condition           = getCondition (sums!!summand)
    result              = Variable "F" /= simplifyExpression dataspec (substituteInExpression subs2 condition)

afterValue :: DataSpec -> Changed -> LPPE -> Int -> Int -> String -> String
afterValue dataspec changed lppe i cfp before | source dataspec lppe i cfp == Value before                         = getUniqueValue (destination dataspec lppe i cfp)
                                              | source dataspec lppe i cfp == Bottom && elem (i, cfp) changed      = getUniqueValue (destination2 dataspec lppe i cfp)
                                              | source dataspec lppe i cfp == Bottom && not (elem (i,cfp) changed) = before

-----------------

cfpPairRulesOutConfluentCycle :: PSpecification -> Changed -> ConfluentSummands -> Int -> Int -> Int -> Bool
cfpPairRulesOutConfluentCycle (lppe,initial,dataspec) changed confluents confNr cfp1 cfp2 
  | before1 /= Bottom && after1 /= Bottom && before2 /= Bottom && after2 /= Bottom && snd (pars!!cfp2) /= TypeName "Queue" && snd (pars!!cfp2) /= TypeName "Nat" = result
  | otherwise                                                                      = False
  where
    before1              = source dataspec lppe confNr cfp1
    (Value beforeValue1) = before1
    after1               = destination dataspec lppe confNr cfp1
    (Value afterValue1)  = after1   
    before2              = source dataspec lppe confNr cfp2
    (Value beforeValue2) = before2
    after2               = destination dataspec lppe confNr cfp2
    (Value afterValue2)  = after2   
    possibleValues       = getPossibleValuesOfCFPPairs lppe dataspec changed (confluents \\ [confNr]) cfp1 cfp2 [(afterValue1, afterValue2)]
    result               = not (elem (beforeValue1, beforeValue2) possibleValues)
    LPPE name pars sums  = lppe
    valuesOfSecond       = getValues dataspec (snd (pars!!cfp2))
    after                = \x -> if elem (confNr, cfp2) changed then getUniqueValue (destination2 dataspec lppe confNr cfp2) else x 
    result2              = [getPossibleValuesOfCFPPairs lppe dataspec changed (confluents \\ [confNr]) cfp1 cfp2 [(afterValue1, after value2)] | value2 <- valuesOfSecond]
 
getPossibleValuesOfCFPPairs :: LPPE -> DataSpec -> Changed -> ConfluentSummands -> Int -> Int -> [(String, String)] -> [(String, String)]
getPossibleValuesOfCFPPairs lppe dataspec changed confluents cfp1 cfp2 values | newValues == values = values
                                                                              | otherwise           = getPossibleValuesOfCFPPairs lppe dataspec changed confluents cfp1 cfp2 newValues
  where
    newValues = nub (values ++ [(afterValue dataspec changed lppe i cfp1 v1, afterValue dataspec changed lppe i cfp2 v2) | i <- confluents, (v1, v2) <- values, enabled dataspec lppe i [(cfp1, v1), (cfp2, v2)]])


checkRules2 :: DataSpec -> LPPE -> Int -> Int -> Bool
checkRules2 dataspec lppe summandNr parNr = (source dataspec lppe summandNr parNr /= Bottom && destination dataspec lppe summandNr parNr /= Bottom)
                                            || (source dataspec lppe summandNr parNr == Bottom && destination2 dataspec lppe summandNr parNr /= Bottom)

getRules2 :: DataSpec -> LPPE -> Rules
getRules2 dataspec lppe = [(summandNr, parNr) | summandNr <- getPSummandNrs lppe, 
                                                   parNr <- [0..length (getLPPEPars lppe) - 1],
                                                   checkRules2 dataspec lppe summandNr parNr]

destination2 :: DataSpec -> LPPE -> Int -> Int -> UniqueValue
destination2 dataspec lppe summandNr parNr = destination
  where
    summand       = getPSummand lppe summandNr
    nextState     = getNextState summand parNr
    nextStateEasy = simplifyExpression dataspec nextState
    destination   = case (nextStateEasy) of
                             (Variable v)         -> if (expressionIsConstant dataspec (Variable v)) then Value v else Bottom
                             (Function name pars) -> Bottom