module MaximalProgress where

import MLPPE
import LPPE 
import Debug.Trace
import Auxiliary
import DataSpec
import Data.Set
import Expressions

-- Removes summands that can never be taken, since they are Markovian and are always enabled
-- together with at least one summand that is interactive.
-- We do not require the interactive summand to have a tau-action, since this reduction
-- is only applied to the MLPPE, and then all actions are considered internal.
-- (Otherwise, add the condition "&& a == "tau"" to conditionImplies c c2 in rateSummandEnabled.)
removeUnnecessaryRates :: MSpecification -> MSpecification
removeUnnecessaryRates (MLPPE name params summands, initialState, dataSpec) = (MLPPE name params newSummands, initialState, dataSpec)
  where
    newSummands = removeUnnecessaryRates2 summands (getActionSummands summands)

-- Removes summands that can never be taken, since they are Markovian and are always enabled
-- together with at least one summand that is interactive.
removeUnnecessaryRates2 :: [GeneralSummand] -> [PSummand] -> [GeneralSummand]
removeUnnecessaryRates2 [] actionSummands                  = []
removeUnnecessaryRates2 ((PSummand p):rest) actionSummands = ((PSummand p):removeUnnecessaryRates2 rest actionSummands)
removeUnnecessaryRates2 ((MSummand m):rest) actionSummands | rateSummandEnabled (MSummand m) actionSummands = ((MSummand m):removeUnnecessaryRates2 rest actionSummands)
                                                           | otherwise                           = removeUnnecessaryRates2 rest actionSummands

-- Checks whether a Markovian summand is possibly sometimes enabled.
-- This is the case if we cannot find an interactive summand that is always
-- enabled at the same time. To check that, we see if we can find an interactive
-- summand whose condition is implied by the condition of the Markovian summand.
rateSummandEnabled (MSummand (params, c, lambda, g)) [] = True
rateSummandEnabled (MSummand (params, c, lambda, g)) ((params2, c2, reward2, a, aps, probChoices, g2):rest) 
  | conditionImplies c c2 = False
  | otherwise             = rateSummandEnabled (MSummand (params, c, lambda, g)) rest

-- Returns all interactive summands.
getActionSummands :: [GeneralSummand] -> [PSummand]
getActionSummands []                  = []
getActionSummands ((MSummand m):rest) = getActionSummands rest
getActionSummands ((PSummand  p):rest) | getAction p /= "reachConditionAction" && getAction p /= "stateRewardAction" = p:(getActionSummands rest)
                                       | otherwise                                                                   = getActionSummands rest