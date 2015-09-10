------------
-- SUMELM --
------------

module Sumelm where

import Auxiliary
import Expressions
import LPPE 
import MLPPE
import DataSpec
import Simplify

-- This function removes unnecessary summations from an LPPE.
sumelm :: PSpecification -> PSpecification
sumelm ((LPPE name pars summands), initialState, dataspec) = (LPPE name pars newSummands, initialState, dataspec)
  where
    newSummands = map (\x -> sumelmPSummand x (map fst (getLocalPars x)) dataspec) summands

-- This function removes unnecessary summations from a summand.
sumelmPSummand :: PSummand -> [Variable] -> DataSpec -> PSummand
sumelmPSummand s [] dataspec = s
sumelmPSummand (params, c, reward, a, aps, probChoices, g) (var:vars) dataspec 
  | variableNotUsed = sumelmPSummand (paramsNew, c, reward, a, aps, probChoices, g) vars dataspec
  | onlyOneValue || singleton = sumelmPSummand (paramsNew, cNew, rewardNew, a, apsNew, probChoices, gNew) vars dataspec
  | otherwise       = sumelmPSummand (params, c, reward, a, aps, probChoices, g) vars dataspec
  where
    paramsNew                = [(v,t) | (v,t) <- params, not (v == var)]
    variableNotUsed          = not(variableInExpression var c) && 
                               not(variableInExpression var reward) && 
							   not(variableInExpressions var aps) &&
                               not(variableInExpressions var g)
    value                    = possibleValueForConditionToBeTrue var c
    onlyOneValue             = value /= Undecided
    typ                      = [t | (v,t) <- params, v == var]!!0
    singleton                = length (getValues dataspec typ) == 1
    UniqueValue uniqueChoic_ = value
    uniqueChoice             = if onlyOneValue then uniqueChoic_ else (Variable ((getValues dataspec typ)!!0))
    cNew                     = substituteInExpression [(var, uniqueChoice)] c
    rewardNew                = substituteInExpression [(var, uniqueChoice)] reward
    apsNew                   = map (substituteInExpression [(var, uniqueChoice)]) aps
    gNew                     = if (elem var (map fst3 probChoices)) then g else (map (substituteInExpression [(var, uniqueChoice)]) g)

--------------------------

sumelmM :: MSpecification -> MSpecification
sumelmM ((MLPPE name pars summands), initialState, dataspec) = (MLPPE name pars newSummands, initialState, dataspec)
  where
    newSummands = map (\x -> sumelmSummandM x dataspec (getMLocalPars x)) summands

sumelmSummandM :: GeneralSummand -> DataSpec -> LocalPars -> GeneralSummand
sumelmSummandM s _ [] = s
sumelmSummandM (PSummand ps) dataspec lp = PSummand (sumelmPSummand ps (map fst lp) dataspec)
sumelmSummandM (MSummand (params, c, lambda, g)) dataspec ((var,typ):vars)
  | variableNotUsed        = sumelmSummandM (MSummand (paramsNew, c, Function "multiply" [Variable (show (length (getValues dataspec typ))), lambda], g)) dataspec vars
  | onlyOneValue           = sumelmSummandM (MSummand (paramsNew, cNew, lambdaNew, gNew)) dataspec vars
  | onlyInRate             = sumelmSummandM (MSummand (paramsNew, c, Function "plus" [substituteInExpression [(var, Variable val)] lambda | val <- (getValues dataspec typ)], g)) dataspec vars -- Note: this is fine since 
  | onlyInRateAndCondition = sumelmSummandM (MSummand (paramsNew, Variable "T", newLambda2, g)) dataspec vars
  | otherwise              = sumelmSummandM (MSummand (params, c, lambda, g)) dataspec vars
  where
    paramsNew                 = [(v,t) | (v,t) <- params, not (v == var)]
    variableNotUsed           = not(variableInExpression var c) && 
                                not(variableInExpression var lambda) &&
                                not(variableInExpressions var g)
    value                     = possibleValueForConditionToBeTrue var c
    onlyOneValue              = value /= Undecided
    UniqueValue uniqueChoice  = value
    cNew                      = substituteInExpression [(var, uniqueChoice)] c
    lambdaNew                 = substituteInExpression [(var, uniqueChoice)] lambda
    gNew                      = map (substituteInExpression [(var, uniqueChoice)]) g
    onlyInRate                = not(variableInExpression var c) &&
                                not(variableInExpressions var g)
    conditionOnlyDependsOnVar = and (map (\x -> x == Variable "T" || x == Variable "F") [simplifyExpression dataspec (substituteInExpression [(var, Variable val)] c) | val <- (getValues dataspec typ)])
    partialRates              = [substituteInExpression [(var, Variable val)] lambda | val <- (getValues dataspec typ), simplifyExpression dataspec (substituteInExpression [(var, Variable val)] c) == Variable "T"]
    newLambda2                = Function "plus" partialRates
    onlyInRateAndCondition    = conditionOnlyDependsOnVar && not(variableInExpressions var g) && length partialRates > 0