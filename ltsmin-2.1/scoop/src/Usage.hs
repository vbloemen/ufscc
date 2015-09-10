-----------------------------------------------
-- This module deals with detecting whether  --
-- parameters are changed or used in an LPPE --
-----------------------------------------------

module Usage where

import LPPE
import Expressions
import Auxiliary
import DataSpec

-- When using these types, [(i,j)] means that parameter j is 
-- changed / used / directly used in summand i.
type Changed      = [(Int, Int)]
type Used         = [(Int, Int)]
type DirectlyUsed = [(Int, Int)]

----------------------------------------------------------------
-- Functions for checking whether or not a parameters is used --
----------------------------------------------------------------

-- This function checks whether a parameter is directly used
-- in a summand, meaning that it is used in either the action
-- parameters, the condition, or the probabilistic choices.
isDirectlyUsedInSummand :: PSummand -> Variable -> Bool
isDirectlyUsedInSummand (params, c, reward, a, aps, probChoices, g) name = directlyUsed
  where
    overwrittenBySum  = elem name (map fst params)
    usedInActionPars  = variableInExpressions name aps
    usedInCondition   = variableInExpression name c
    usedInReward      = variableInExpression name reward
    overwrittenByProb = elem name (map fst3 probChoices)
    usedInProb        = variableInExpressions name (map thd3 probChoices)
    directlyUsed      = not overwrittenBySum && (usedInActionPars || usedInCondition || usedInReward ||  
                                                 (not overwrittenByProb && usedInProb))

-- This function checks whether a parameter is used either directly,
-- or in the next state expression of another parameter.
--
-- The boolean parameter indicates whether or not a parameter is considered
-- used in a summand when it is only used to update itself.
isUsedInSummand :: PSummand -> Int -> Variable -> Bool -> Bool
isUsedInSummand (params, c, reward, a, aps, probChoices, g) i name self = used
  where
    overwrittenBySum  = elem name (map fst params)
    usedInActionPars  = variableInExpressions name aps
    usedInCondition   = variableInExpression name c
    usedInReward      = variableInExpression name reward
    overwrittenByProb = elem name (map fst3 probChoices)
    usedInProb        = variableInExpressions name (map thd3 probChoices)
    usedInNextState   = variableInExpressions name ([g!!j | j <- [0..i-1]] ++ [g!!j | j <- [i+1..length(g)-1]])
                         || (self && variableInExpression name (g!!i) && not((g!!i) == Variable name))
    used              = not overwrittenBySum && (usedInActionPars || usedInCondition || usedInReward || 
                                                (not overwrittenByProb && (usedInProb || usedInNextState)))

-- This function checks whether a parameter is changed in a summand.
-- This is the case when either the next state is an expression different
-- from just the parameter name itself, or is the parameter is used in a 
-- nondeterministic or probabilistic sum.
isChangedInSummand :: LPPE -> Int -> Int -> Bool
isChangedInSummand lppe summandNr parNr = nextState /= Variable parameter 
                                          || elem parameter (map fst params) 
                                          || elem parameter (map fst3 probChoices)
  where
    summand   = getPSummand lppe summandNr
    nextState = getNextState summand parNr
    parameter = fst ((getLPPEPars lppe)!!parNr)
    (params, c, reward, a, aps, probChoices, g) = summand

-- Provides a list of elements of the form (i,j),
-- indicating that summand i changes parameter j.
-- For every pair (i,j) that is NOT in this list,
-- summand i does not change parameter j.
--
-- Note that the result of this function is an overapproximation
-- of the variables that are actually changed, as heuristics
-- are used to detect whether or not a variable will be changed.
-- It could be the case the a pair (i,j) is present in the
-- result, even though summand i will in fact never actually
-- change parameter j.
getChanged :: LPPE -> Changed
getChanged lppe = [(summandNr, parNr) | summandNr <- getPSummandNrs lppe, 
                                        parNr <- [0..length (getLPPEPars lppe) - 1],
                                        isChangedInSummand lppe summandNr parNr]

-- Provides a list of elements of the form (i,j),
-- indicating that summand i uses parameter j.
-- For every pair (i,j) that is NOT in this list,
-- summand i does not uses parameter j.
--
-- Note that the result of this function is an overapproximation
-- of the variables that are actually used, as heuristics
-- are used to detect whether or not a variable will be used.
-- It could be the case the a pair (i,j) is present in the
-- result, even though summand i will in fact never actually
-- use parameter j.
getUsed :: LPPE -> Used
getUsed lppe = [(summandNr, parNr) | summandNr <- getPSummandNrs lppe, 
                                     parNr <- getUsedInSummand lppe summandNr
               ]

-- Provides a list of elements of the form (i,j),
-- indicating that summand i directly uses parameter j.
-- For every pair (i,j) that is NOT in this list,
-- summand i does not directly use parameter j.
--
-- Note that the result of this function is an overapproximation
-- of the variables that are actually directly used, as heuristics
-- are used to detect whether or not a variable will be directly used.
-- It could be the case the a pair (i,j) is present in the
-- result, even though summand i will in fact never actually
-- use parameter j directly.
getDirectlyUsed :: LPPE -> DirectlyUsed
getDirectlyUsed lppe = [(summandNr, parNr) | summandNr <- getPSummandNrs lppe, 
                                             parNr <- getDirectlyUsedInSummand lppe summandNr]

getDirectlyUsedInSummand :: LPPE -> Int -> [Int]
getDirectlyUsedInSummand (LPPE name pars summands) i = getDirectlyUsedInSummand2 (summands!!i) (zip [0..length(pars) - 1] (map fst pars))

getDirectlyUsedInSummand2 :: PSummand -> [(Int, Variable)] -> [Int]
getDirectlyUsedInSummand2 summand [] = []
getDirectlyUsedInSummand2 summand ((i,name):is) | isDirectlyUsedInSummand summand name = i:(getDirectlyUsedInSummand2 summand is)
                                                | otherwise                           = getDirectlyUsedInSummand2 summand is

getChangedInNextStateNums :: PSummand -> [(Int, Variable)] -> [Int]
getChangedInNextStateNums summand [] = []
getChangedInNextStateNums summand ((i,name):is) | isChangedInNextState summand i name = i:(getChangedInNextStateNums summand is)
                                                | otherwise                           = getChangedInNextStateNums summand is

isChangedInNextState :: PSummand -> Int -> Variable -> Bool
isChangedInNextState (params, c, reward, a, aps, probChoices, g) i name = not((g!!i) == Variable name) || (elem name (map fst params)) || (elem name (map fst3 probChoices))

getUsedInSummand :: LPPE -> Int -> [Int]
getUsedInSummand (LPPE name pars summands) i = getUsedInSummand2 (summands!!i) (zip [0..length(pars) - 1] (map fst pars))

getUsedInSummand2 :: PSummand -> [(Int, Variable)] -> [Int]
getUsedInSummand2 summand [] = []
getUsedInSummand2 summand ((i,name):is) | isUsedInSummand summand i name True = i:(getUsedInSummand2 summand is)
                                                | otherwise                           = getUsedInSummand2 summand is

getChangedUnchanged :: LPPE -> Int -> [Int]
getChangedUnchanged (LPPE name pars summands) i = getChangedUnchanged2 (summands!!i) (zip [0..length(pars) - 1] (map fst pars))

-- Dit nog verbeteren: hoeft niet perse een getal te zijn. Maar, mag niet in (prob) sum een waarde gekregen hebben.
-- We checken of een variabele alleen maar 'veranderd' wordt in wat ie toch al moest zijn.
getChangedUnchanged2 :: PSummand -> [(Int, Variable)] -> [Int]
getChangedUnchanged2 summand [] = []
getChangedUnchanged2 summand ((i,name):is) | changedUnchanged  = i:(getChangedUnchanged2 summand is)
                                                         | otherwise = getChangedUnchanged2 summand is
  where
    next              = getNextState summand i
    isValue           = case next of
      Variable val -> isInteger val
      otherwise    -> False
    values            = possibleValueForConditionToBeTrue name (getCondition summand)
    UniqueValue value = values
    changedUnchanged  = isValue && values /= Undecided && value == next



getUsedOnlyInGreaterThanCondition :: LPPE -> Int -> [Int]
getUsedOnlyInGreaterThanCondition (LPPE name pars summands) i = getUsedOnlyInGreaterThanCondition2 (summands!!i) (zip [0..length(pars) - 1] (map fst pars))

getUsedOnlyInGreaterThanCondition2 :: PSummand -> [(Int, Variable)] -> [Int]
getUsedOnlyInGreaterThanCondition2 summand [] = []
getUsedOnlyInGreaterThanCondition2 summand ((i,name):is) | usedOnly  = i:(getUsedOnlyInGreaterThanCondition2 summand is)
                                                         | otherwise = getUsedOnlyInGreaterThanCondition2 summand is
  where
    usedOnly = variableInExpressionOnlyGreaterEqual name (getCondition summand)

getRestrictedChanged :: LPPE -> Int -> [Int]
getRestrictedChanged (LPPE name pars summands) i = getRestrictedChanged2 (summands!!i) (zip [0..length(pars) - 1] (map fst pars))

getRestrictedChanged2 :: PSummand -> [(Int, Variable)] -> [Int]
getRestrictedChanged2 summand [] = []
getRestrictedChanged2 summand ((i,name):is) | result  = i:(getRestrictedChanged2 summand is)
                                            | otherwise = getRestrictedChanged2 summand is
  where
    (params, c, reward, a, aps, probChoices, g) = summand
    nextState = getNextState summand i
    result = not(elem name (map fst params)) && not (elem name (map fst3 probChoices))
           && (nextState == Variable name || isAddition nextState name)

isAddition (Function "plus" [var, Variable val]) name | var == Variable name && isInteger val && parseInteger val >= 0 = True
isAddition _ _                             = False

isSubtraction (Function "minus" [var, Variable val]) name | var == Variable name && isInteger val && parseInteger val >= 0 = True
isSubtraction _ _                                                                                                          = False


getUsedInAction :: LPPE -> Int -> [Int]
getUsedInAction (LPPE name pars summands) i = getUsedInAction2 (summands!!i) (zip [0..length(pars) - 1] (map fst pars))

getUsedInAction2 :: PSummand -> [(Int, Variable)] -> [Int]
getUsedInAction2 summand [] = []
getUsedInAction2 summand ((i,name):is) | used      = i:(getUsedInAction2 summand is)
                                       | otherwise = getUsedInAction2 summand is
  where
    (params, c, reward, a, aps, probChoices, g) = summand
    overwrittenBySum                            = elem name (map fst params)
    used                                        = not(overwrittenBySum) && variableInExpressions name aps


getUsedInReward :: LPPE -> Int -> [Int]
getUsedInReward (LPPE name pars summands) i = getUsedInReward2 (summands!!i) (zip [0..length(pars) - 1] (map fst pars))

getUsedInReward2 :: PSummand -> [(Int, Variable)] -> [Int]
getUsedInReward2 summand [] = []
getUsedInReward2 summand ((i,name):is) | used      = i:(getUsedInReward2 summand is)
                                       | otherwise = getUsedInReward2 summand is
  where
    (params, c, reward, a, aps, probChoices, g) = summand
    overwrittenBySum                            = elem name (map fst params)
    used                                        = not(overwrittenBySum) && variableInExpression name reward


getUsedInNext :: LPPE -> Int -> [Int]
getUsedInNext (LPPE name pars summands) i = getUsedInNext2 (summands!!i) (zip [0..length(pars) - 1] (map fst pars))

getUsedInNext2 :: PSummand -> [(Int, Variable)] -> [Int]
getUsedInNext2 summand [] = []
getUsedInNext2 summand ((i,name):is) | used      = i:(getUsedInNext2 summand is)
                                       | otherwise =  getUsedInNext2 summand is
  where
    (params, c, reward, a, aps, probChoices, g) = summand
    overwrittenBySum                    = elem name (map fst params)
    usedInNextState                     = variableInExpressions name ([g!!j | j <- [0..i-1]] ++ [g!!j | j <- [i+1..length(g)-1]])
                                          || (variableInExpression name (g!!i) && not((g!!i) == Variable name))
    overwrittenByProb                   = elem name (map fst3 probChoices)
    used                                = not(overwrittenBySum) && not(overwrittenByProb) && usedInNextState


getUsedInCondition :: LPPE -> Int -> [Int]
getUsedInCondition (LPPE name pars summands) i = getUsedInCondition2 (summands!!i) (zip [0..length(pars) - 1] (map fst pars))

getUsedInCondition2 :: PSummand -> [(Int, Variable)] -> [Int]
getUsedInCondition2 summand [] = []
getUsedInCondition2 summand ((i,name):is) | used      = i:(getUsedInCondition2 summand is)
                                       | otherwise = getUsedInCondition2 summand is
  where
    (params, c, reward, a, aps, probChoices, g) = summand
    overwrittenBySum                    = elem name (map fst params)
    used                                = not(overwrittenBySum) && variableInExpression name c

getChangedLinear  :: LPPE -> Int -> [Int]
getChangedLinear (LPPE name pars summands) i = getChangedLinear2 (summands!!i) (zip [0..length(pars) - 1] (map fst pars))

getChangedLinear2 :: PSummand -> [(Int, Variable)] -> [Int]
getChangedLinear2 summand [] = []
getChangedLinear2 summand ((i,name):is) | changedLinear && not(usedElsewhere) = i:(getChangedLinear2 summand is)
                                        | otherwise                           =   getChangedLinear2 summand is
  where
    (params, c, reward, a, aps, probChoices, g) = summand
    nextState                           = g!!i
    changedLinear                       = isLinear nextState name
    overwrittenBySum                    = elem name (map fst params)
    overwrittenByProb                   = elem name (map fst3 probChoices)
    usedInNextState                     = variableInExpressions name ([g!!j | j <- [0..i-1]] ++ [g!!j | j <- [i+1..length(g)-1]])
    usedElsewhere                       = not(overwrittenBySum) && not(overwrittenByProb) && usedInNextState


isLinear (Function "plus" [Variable l, Variable r]) name = l == name && isNumber r
isLinear (Function "minus"  [Variable l, Variable r]) name = l == name && isNumber r
isLinear _ _                                             = False

getUsedInProbs :: LPPE -> Int -> [Int]
getUsedInProbs (LPPE name pars summands) i = getUsedInProbs2 (summands!!i) (zip [0..length(pars) - 1] (map fst pars))

getUsedInProbs2 :: PSummand -> [(Int, Variable)] -> [Int]
getUsedInProbs2 summand [] = []
getUsedInProbs2 summand ((i,name):is) | used      = i:(getUsedInProbs2 summand is)
                                       | otherwise = getUsedInProbs2 summand is
  where
    (params, c, reward, a, aps, probChoices, g) = summand
    overwrittenBySum                    = elem name (map fst params)
    overwrittenByProb                   = elem name (map fst3 probChoices)
    used                                = not(overwrittenBySum) && not(overwrittenByProb) && variableInExpressions name (map thd3 probChoices)
