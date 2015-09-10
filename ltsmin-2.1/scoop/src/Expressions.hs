-----------------------------------------------
-- Helper functions to work with expressions --
--                                           --
-- Finished refactoring: 17-sep-2010         -- 
-----------------------------------------------

module Expressions where

import Auxiliary
import Data.List
import DataSpec


type Substitution = (Variable, Expression)

data PossibleValues = Undecided | UniqueValue Expression | PossibleValues [Expression] deriving Eq

-- This function prints an expression in prCRL-style
printExpression :: Expression -> String
printExpression (Variable variable)   = variable
printExpression (Function name pars)  = name ++ optionalParentheses (infixString (map printExpression pars) ", ")

-- This function checks whether a variable occurs in a list of expressions.
variableInExpressions :: Variable -> [Expression] -> Bool
variableInExpressions var expressions = or (map (variableInExpression var) expressions)

-- This function checks whether a variable occurs in an expression.
variableInExpression :: Variable -> Expression -> Bool
variableInExpression var (Variable v)      | var == v  = True
                                           | otherwise = False
variableInExpression var (Function f pars)             = variableInExpressions var pars

-- This function results true if a variable only occurs in a greater than or greater than or equal function.
variableInExpressionOnlyGreaterEqual :: Variable -> Expression -> Bool
variableInExpressionOnlyGreaterEqual var (Function "geq" [v,val])      = (v == Variable var && not (variableInExpression var val))
                                                                         || (not (variableInExpression var val) &&
                                                                             not (variableInExpression var v))
variableInExpressionOnlyGreaterEqual var (Function "gt" [v,val])       = (v == Variable var && not (variableInExpression var val))
                                                                         || (not (variableInExpression var val) &&
                                                                             not (variableInExpression var v))
variableInExpressionOnlyGreaterEqual var (Function "and" pars)         = and (map (variableInExpressionOnlyGreaterEqual var) pars)
variableInExpressionOnlyGreaterEqual var (Function "or" pars)          = and (map (variableInExpressionOnlyGreaterEqual var) pars)
variableInExpressionOnlyGreaterEqual var (Function f pars)             = not (or (map (variableInExpression var) pars))
variableInExpressionOnlyGreaterEqual var (Variable v)      | var == v  = False
                                                           | otherwise = True

variablesInFunction :: Variable -> Variable -> Expression -> Bool
variablesInFunction var var2 (Variable v)      = False
variablesInFunction var var2 (Function f pars) = (elem (Variable var) pars && elem (Variable var2) pars)
                                                 || or (map (variablesInFunction var var2) pars)

getExpressionSize :: Expression -> Int
getExpressionSize (Variable v)      = 1
getExpressionSize (Function f pars) = 1 + sum (map getExpressionSize pars)

functionsInExpression :: Expression -> [String]
functionsInExpression (Variable v) = []
functionsInExpression (Function name pars) = name : concat (map functionsInExpression pars)

-- This function takes a list of substitutions, and an expression.
-- In the expression, it substitutes each variable's value for it.
substituteInExpression :: [Substitution] -> Expression -> Expression
substituteInExpression subs (Function f pars) = Function f (map (substituteInExpression subs) pars)
substituteInExpression subs (Variable v) | elem v (map fst subs) && length values > 1  = error("Error: Nondeterministic substitution applied: " ++ v ++ " -> " ++ show values)
                                         | elem v (map fst subs) && length values == 1 = values!!0
                                         | otherwise             = Variable v
  where
    values = [val | (var,val) <- subs, var == v]

-- This function checks whether an expression is a constant.
expressionIsConstant :: DataSpec -> Expression -> Bool
expressionIsConstant dataspec (Function name pars) = False
expressionIsConstant dataspec (Variable v)         = stringIsConstant dataspec v -- || isInteger -- || isFraction
--  where
--    nr        = takeWhile(\x -> x == '0' || x == '1' || x == '2' || x == '3' || x == '4' || x == '5' || x == '6' || x == '7' || x == '8' || x == '9' ) 
--                          (if (head v == '-') then tail v else v)  
--    isInteger = (length nr > 0) && ((nr == v) || ((head v == '-') && ('-':nr) == v))


-- The function assesses whether there is only a single possible values for a variable
-- for a boolean expression to evaluate to true. In case no conclusive appromation can 
-- be made, the verdict 'Undecided' is returned.
possibleValueForConditionToBeTrue :: Variable -> Expression -> PossibleValues
possibleValueForConditionToBeTrue var expression = possibleValue
  where
    possibleValues = possibleValuesForConditionToBeTrue var expression
    possibleValue  = case possibleValues of
      PossibleValues [value] -> UniqueValue value
      otherwise              -> Undecided

-- This function assesses the possible values of a variable for a boolean expression
-- to evaluate to true. In case no conclusive appromation can be made,
-- the verdict 'Undecided' is returned.
possibleValuesForConditionToBeTrue :: Variable -> Expression -> PossibleValues
possibleValuesForConditionToBeTrue var (Function "eq" [Variable a, x]) 
  | a == var && not(variableInExpression var x) = PossibleValues [x]
possibleValuesForConditionToBeTrue var (Function "eq" [x, Variable a]) 
  | a == var && not(variableInExpression var x) = PossibleValues [x]
possibleValuesForConditionToBeTrue var (Function "and" pars)                     = conjunctionPossibleValues (map (possibleValuesForConditionToBeTrue var) pars)
possibleValuesForConditionToBeTrue var (Function "or" pars)                      = disjunctionPossibleValues (map (possibleValuesForConditionToBeTrue var) pars)
possibleValuesForConditionToBeTrue var (Function "not" [Variable a]) | a == var  = PossibleValues [Variable "F"]
possibleValuesForConditionToBeTrue var (Variable v)                  | v == var  = PossibleValues [Variable "T"]
possibleValuesForConditionToBeTrue var _                                         = Undecided

-- This function combines the possible values for the parts of a conjunction 
-- to be true into the possible values for the conjunction in total to be true. 
conjunctionPossibleValues :: [PossibleValues] -> PossibleValues
conjunctionPossibleValues []                          = Undecided
conjunctionPossibleValues [Undecided]                 = Undecided
conjunctionPossibleValues [PossibleValues values]     = PossibleValues values
conjunctionPossibleValues ((Undecided):ps)            = conjunctionPossibleValues ps
conjunctionPossibleValues ((PossibleValues values):ps) = combine
  where
    rest    = conjunctionPossibleValues ps
    combine = case rest of
                Undecided                -> PossibleValues values
                (PossibleValues values2) -> PossibleValues (intersect values values2)

-- This function combines the possible values for the parts of a disjunction 
-- to be true into the possible values for the disjunction in total to be true. 
disjunctionPossibleValues :: [PossibleValues] -> PossibleValues
disjunctionPossibleValues []                          = Undecided
disjunctionPossibleValues [Undecided]                 = Undecided
disjunctionPossibleValues [PossibleValues values]     = PossibleValues values
disjunctionPossibleValues ((Undecided):ps)            = Undecided
disjunctionPossibleValues ((PossibleValues values):ps) = result
   where
     rest = disjunctionPossibleValues ps
     result = case rest of
                Undecided                -> Undecided
                (PossibleValues values2) -> PossibleValues (union values values2)


-- Creates a condition that always holds in the next state after a summand 
makeCondition [(var,typ)] [expr]            = Function "eq" [Variable var, expr]
makeCondition ((var,typ):rest) (expr:rest2) = Function "and" [Function "eq" [Variable var, expr], makeCondition rest rest2]
makeCondition [] []                         = Variable "true"
makeCondition x y                           = error("Error in function makeCondition")

-- Checks whether the condition given as the first parameter implies the condition
-- that is given as the second parameter.
conditionImplies :: Expression -> Expression -> Bool
conditionImplies c c2 | c == c2                             = True
conditionImplies (Function "eq" [Variable a,Variable b])
  (Function "not" [Function "eq" [Variable c, Variable d]])     
  | a == c && b /= d && a /= b && c /= d                    = True
conditionImplies (Function "not" [c]) (Function "not" [c2]) = conditionImplies c2 c
conditionImplies c (Function "and" conjuncts2)              = and (Prelude.map (conditionImplies c) conjuncts2)
conditionImplies c (Function "or" conjuncts2)               = or (Prelude.map (conditionImplies c) conjuncts2)
conditionImplies (Function "and" conjuncts) c2              = or [conditionImplies c c2 | c <- conjuncts]
conditionImplies (Function "or" conjuncts) c2               = and [conditionImplies c c2 | c <- conjuncts]
conditionImplies c c2                                       = False