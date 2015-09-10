module Linearise where

import Auxiliary
import LPPE
import Processes
import Expressions
import Regularise
import DataSpec

linearise :: DataSpec -> [Process] -> InitialState -> (LPPE, InitialState)
linearise dataspec processes initialState = (LPPE "X" pars (makeSummands procs), newInitialState)
                     where
                       (procs, initState) = toIntermediateForm processes 0 initialState (arbitraryConstant dataspec)
                       nrStates           = length procs
                       pars               = ("pc", TypeRange 1 nrStates):getProcPars (procs!!0)
                       newInitialState    = "1":(map snd initState)


makeSummands :: [Process] -> [PSummand]
makeSummands []                           = []
makeSummands ((Process name pars rhs):ps) = (makeSummand (tail name) rhs) ++ (makeSummands ps)

makeSummand :: [Char] -> ProcessTerm -> [PSummand]
makeSummand pc (Sum var typ rhs)              = map (addSummation [(var, typ)]) (makeSummand pc rhs) 
makeSummand pc (Implication cond rhs)         | correctSummations rhs cond = map (addImplication cond) (makeSummand pc rhs)
makeSummand pc (Plus rhss)                    = concat (map (makeSummand pc) rhss)
makeSummand pc (ActionPrefix reward a aps probs rhs) = [([], Function "eq" [Variable "pc", Variable pc], reward, a, aps, probs, nextState)]
  where
    (ProcessInstance name actualPars) = rhs
    actualPars2 = (Variable (tail name)):actualPars
    nextState = actualPars2
makeSummand pc other                          = error("Linearisation error: RHS not in IRF.")

arbitraryConstant :: DataSpec -> Type -> String
arbitraryConstant dataspec sort = (allValues sort (fst3 dataspec))!!0

-- This function checks whether linearisation can be performed.
-- This function should always return True, otherwise there is a mistake
-- in the procedure that produces the IRF. However, for testing purposes
-- we retain this function.
correctSummations :: ProcessTerm -> Expression -> Bool
correctSummations rhs (Variable d) = correctRHS d rhs
correctSummations rhs (Function f []) = True
correctSummations rhs (Function f pars) = and [correctSummations rhs p | p <- pars]

-- Checks whether a RHS is correct in the sense that it does not contain a summation over a d
-- (to make sure that summations and implications are not erroneously interchanged.)
correctRHS :: String -> ProcessTerm -> Bool
correctRHS d (ProcessInstance name pars)       = True
correctRHS d (ActionPrefix reward act pars probs rhs) = True
correctRHS d (Implication cond rhs)            = correctRHS d rhs
correctRHS d (Plus rhss)                       = and (map (correctRHS d) rhss)
correctRHS d (Sum v typ rhs) | d == v          = error("Error: Linearisation failed.\nIt tried to interchange a summation and an implication which both contained the same variable name.\nPlease rename variable " ++ d ++ " in sum(" ++ v ++ ":" ++ printType typ ++ ", ...) to something that is not already a global variable.")
                             | otherwise       = correctRHS d rhs