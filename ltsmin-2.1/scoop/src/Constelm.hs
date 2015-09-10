----------------------
--     CONSTELM     --
----------------------

module Constelm where

import Auxiliary
import LPPE
import Simplify
import Data.List
import Usage
import DataSpec

-- This function takes a specification, detects which parameters are constant,
-- substitutes the initial values for these parameters wherever they are used,
-- and finally removes the parameters from the LPPE. 
constelm :: PSpecification -> PSpecification
constelm (lppe, initial, dataspec) = removeParametersFromLPPE (lppeReduced, initial, dataspec) (map fst constants)
  where
    parNames     = map fst (getLPPEPars lppe)
    parTypes     = map snd (getLPPEPars lppe)
    allVariables = [(p,parNames!!p,i) | (p, i) <- (zip [0..length initial - 1] initial)]
    singletons   = [(p,parNames!!p,i) | (p, i) <- (zip [0..length initial - 1] initial), length (getValues dataspec (parTypes!!p)) == 1, (getValues dataspec (parTypes!!p)) /= ["empty"], parTypes!!p /= TypeName "Nat"]
    constants    = [(p,i) | (p,s,i) <- (allVariables \\ ((getNonConstants lppe allVariables) \\ singletons))]
    lppeReduced  = substituteInLPPE lppe constants

-- This function takes an LPPE, a list of all parameters of that LPPE in the form (parNr, parName, initValue)
-- and the initial state. It provides a list of parameters of the same form, indicating that
-- those are changed.
getNonConstants :: LPPE -> [(Int, String, String)] -> [(Int, String, String)]
getNonConstants lppe stillConstant | nonConstants == [] = [] 
                                   | otherwise          = nonConstants ++ getNonConstants lppe (stillConstant \\ nonConstants)
  where
    nonConstants = getNonConstants2 lppe stillConstant stillConstant

-- Given an LPPE, a list of parameters (parNr, parName, initValue) that have not been detected to have been changed yet,
-- an a list of all parameters, this function provides a new list of parameters that are not constant.
getNonConstants2 :: LPPE -> [(Int, String, String)] -> [(Int, String, String)] -> [(Int, String, String)]
getNonConstants2 lppe stillConstant [] = []
getNonConstants2 lppe stillConstant ((parNr, name, initialValue):pairs) | isNotConstant = (parNr, name, initialValue):(getNonConstants2 lppe stillConstant pairs)
                                                                       | otherwise     = getNonConstants2 lppe stillConstant pairs
  where
    isNotConstant = or [isNotConstantInSummand lppe stillConstant initialValue summandNr parNr | summandNr <- getPSummandNrs lppe]

-- The function computes whether a parameter isn't constant. This is the case when it is changed, and moreover
-- its next value is not equal to its initial value. Finally, it is also still considered constant when it is
-- to obtain the value of another parameter, that is still considered constant, and has the same initial value
isNotConstantInSummand :: LPPE -> [(Int, String, String)] -> String -> Int -> Int -> Bool
isNotConstantInSummand lppe stillConstant initialValue summandNr parNr = changed && nextState /= Variable initialValue && not(changedToSameConstant) && enabled
  where
    changed               = isChangedInSummand lppe summandNr parNr
    nextState             = simplifyExpression ([],[],[]) (getNextState summand parNr)
    enabled               = simplifyExpression ([],[],[]) (getCondition summand) /= Variable "F"
    summand               = getPSummand lppe summandNr
    probChoices           = getProbChoices summand
    params                = getLocalPars summand
    changedToSameConstant = [ p | (p, s, i) <- stillConstant, nextState == Variable s, i == initialValue, not(elem s (map fst params)), not(elem s (map fst3 probChoices))] /= []