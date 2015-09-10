----------------
--- SIMPLIFY ---
----------------

module Simplify where

import Auxiliary
import LPPE
import Expressions
import DataSpec
import Data.List
import Processes
import Debug.Trace

type TypeMapping    = [(Variable, Type)]

-- This function simplifies a specification by evaluating functions and removing 
-- unaccessible summands.
simplify :: PSpecification -> PSpecification
simplify (LPPE name pars summands, initialState, dataspec) = (LPPE name pars summandsNew, initialState, dataspec)
    where
       summands2    = map (simplifySummand dataspec) summands
       summandsNew  = [summand | summand <- summands2, summandReachable dataspec (pars ++ getLocalPars summand) summand, not((getCondition summand) == Variable "F"), not (disabledByAll pars initialState summand summands2),
                                                       not (getAction summand == "rate" && getActionPars summand == [Variable "0"])]

summandReachable :: DataSpec -> TypeMapping -> PSummand -> Bool
summandReachable dataspec mapping summand = isReachable
  where
    c           = getCondition summand
    isReachable = and [typ == TypeName "Queue" || typ == TypeName "Nat" || nub [simplifyExpression dataspec (substituteInExpression [(par, Variable value)] c) | value <- getValues dataspec typ] /= [Variable "F"] | (par,typ) <- mapping]

simplifySummand :: DataSpec -> PSummand -> PSummand
simplifySummand dataspec (params, c, reward, a, aps, probChoices, g)  = (params, c2, reward2, a, aps2, probChoices2, g2)
     where
       c2   = simplifyExpression dataspec c
       reward2 = simplifyExpression dataspec reward
       aps2 = map (simplifyExpression dataspec) aps
       g2   = map (simplifyExpression dataspec) g
       probChoices2 = simplifyProbChoices dataspec probChoices

simplifyProbChoices dataspec [] = []
simplifyProbChoices dataspec ((v,t,f):rest) = (v,t,f2):(simplifyProbChoices dataspec rest)
  where
    f2   = simplifyExpression dataspec f
    
-- This function simplifies an expression. 
simplifyExpression :: DataSpec -> Expression -> Expression
simplifyExpression dataspec (Variable v)                               = Variable v
simplifyExpression dataspec (Function "plus" [x,y]) 
  | simplifyExpression dataspec x == Variable "0"                      = simplifyExpression dataspec y
  | simplifyExpression dataspec y == Variable "0"                      = simplifyExpression dataspec x
  | simplifyExpression dataspec x == simplifyExpression dataspec y     = simplifyExpression dataspec (Function "multiply" [Variable "2", simplifyExpression dataspec x])
simplifyExpression dataspec (Function "minus" [x,y]) 
  | simplifyExpression dataspec x == simplifyExpression dataspec y     = Variable "0"
  | simplifyExpression dataspec y == Variable "0"                      = simplifyExpression dataspec x
simplifyExpression dataspec (Function "multiply" [x,y]) 
  |  simplifyExpression dataspec x == Variable "0"
  || simplifyExpression dataspec y == Variable "0"                     = Variable "0"
  |  simplifyExpression dataspec x == Variable "1"                     = simplifyExpression dataspec y
  |  simplifyExpression dataspec y == Variable "1"                     = simplifyExpression dataspec x
simplifyExpression dataspec (Function "power" [x,y]) 
  |  simplifyExpression dataspec y == Variable "1"                     = simplifyExpression dataspec x
  |  simplifyExpression dataspec y == Variable "0"                     = Variable "1"
simplifyExpression dataspec (Function "eq" [x,y]) 
  | simplifyExpression dataspec x == simplifyExpression dataspec y     = Variable "T"
simplifyExpression dataspec (Function "and" [par])                     = simplifyExpression dataspec par
simplifyExpression dataspec (Function "not" [Function "not" [par]])    = simplifyExpression dataspec par
simplifyExpression dataspec (Function "not" [Function "and" pars])     = simplifyExpression dataspec new
  where
    new = Function "or" [Function "not" [x] | x <- pars]
simplifyExpression dataspec (Function "not" [Function "or" pars] )     = simplifyExpression dataspec new
  where
    new = Function "and" [Function "not" [x] | x <- pars]
simplifyExpression dataspec (Function "ifthenelse" [c, a, b])
  | isTrue                                                      = simplifyExpression dataspec a
  | isFalse                                                     = simplifyExpression dataspec b
  | simpleA == simpleB                                          = simpleA
  where
    isTrue     = simplifyExpression dataspec c == Variable "T"
    isFalse    = simplifyExpression dataspec c == Variable "F" 
    simpleA    = simplifyExpression dataspec a
    simpleB    = simplifyExpression dataspec b
simplifyExpression dataspec (Function "and" pars) | containsAnd = simplifyExpression dataspec (Function "and" simple)
  where
    (containsAnd, simple) = containsAndExpression pars
simplifyExpression dataspec (Function "and" pars) 
  | elem (Variable "F") pars2                                   = Variable "F"
  | nonTrue == []                                               = Variable "T"
  | length nonTrue == 1                                         = nonTrue!!0
  | otherwise                                                   = Function "and" nonTrue
  where
    pars2   = map (simplifyExpression dataspec) pars
    nonTrue = [p | p <- pars2, p /= Variable "T"]
simplifyExpression dataspec (Function "or" pars) | containsOr = simplifyExpression dataspec (Function "or" simple)
  where
    (containsOr, simple) = containsOrExpression pars
simplifyExpression dataspec (Function "or"  [par])                 = simplifyExpression dataspec par
simplifyExpression dataspec (Function "or" pars) 
  | elem (Variable "T") pars2                                = Variable "T"
  | nonFalse == []                                              = Variable "F"
  | length nonFalse == 1                                        = nonFalse!!0
  | otherwise                                                   = Function "or" nonFalse
  where
    pars2    = map (simplifyExpression dataspec) pars
    nonFalse = [p | p <- pars2 , p /= Variable "F"]
simplifyExpression dataspec (Function "get" [Variable v,Variable n]) | isInteger n && elem ';' v = Variable (evaluateFunction "get" [v,n] (thd3 dataspec))
simplifyExpression dataspec (Function "set" [Variable v,Variable e, Variable n]) | isInteger n && elem ';' v = Variable (evaluateFunction "set" [v,e,n] (thd3 dataspec))
simplifyExpression dataspec (Function name pars) | evaluatable  = Variable (evaluateFunction name (map constantExpressionToString simplePars) (thd3 dataspec))
                                                 | otherwise    = Function name simplePars
  where
    simplePars   = map (simplifyExpression dataspec) pars
    evaluatable  = and (map (expressionIsConstant dataspec) simplePars)

containsOrExpression [] = (False, [Variable ""])
containsOrExpression ((Function "or" p):y) = (True, p ++ y)
containsOrExpression ((y:z))               | not(contains) = (False, [Variable ""]) 
                                 | otherwise     = (True, (y:simple))
  where
    (contains,simple) = containsOrExpression z

containsAndExpression [] = (False, [Variable ""])
containsAndExpression ((Function "and" p):y) = (True, p ++ y)
containsAndExpression ((y:z))    | not(contains) = (False, [Variable ""]) 
                                 | otherwise     = (True, (y:simple))
  where
    (contains,simple) = containsAndExpression z

constantExpressionToString :: Expression -> [Char]
constantExpressionToString (Variable v) = v
constantExpressionToString other        = error("Function constantExpressionToString applied to non-constant.")

disabledByAll :: ProcessPars -> InitialState -> PSummand -> [PSummand] -> Bool
disabledByAll pars initialState summand summands = conditionImplies (makeCondition pars [Variable i | i <- initialState]) (Function "not" [getCondition summand])
                                                 && (and [conditionImplies (makeConditionLPPE s pars) (Function "not" [getCondition summand]) | s <- summands, s /= summand])