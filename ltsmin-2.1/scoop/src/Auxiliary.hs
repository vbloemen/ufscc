----------------------------------------------------------------
-- Provides some auxiliary functions, mainly on strings,      --
-- that are used on several occasions throughout the program. --
--                                                            --
-- Finished refactoring: 16-sep-2010                          --
----------------------------------------------------------------

module Auxiliary where

import Debug.Trace

-- Note: because of this visualisation of expression,
-- the output of the LPPE is not parsable again.
type Variable     = String
type Function     = String
data Expression = Variable Variable | Function Function [Expression] deriving (Eq, Ord) 
instance Show Expression where
  show expression               = if result!!0 == '(' then take (length result - 2) (drop 1 (result)) else result
    where
      result = showExpression expression

showExpression (Variable variable) = variable
showExpression (Function "eq" [x,y]) = "(" ++ showExpression x ++ " = " ++ showExpression y ++ ")"
showExpression (Function "lt" [x,y]) = "(" ++ showExpression x ++ " < " ++ showExpression y ++ ")"
showExpression (Function "plus" [x,y]) = "(" ++ showExpression x ++ " + " ++ showExpression y ++ ")"
showExpression (Function "minus" [x,y]) = "(" ++ showExpression x ++ " - " ++ showExpression y ++ ")"
showExpression (Function "multiply" [x,y]) = "(" ++ showExpression x ++ " * " ++ showExpression y ++ ")"
showExpression (Function "power" [x,y]) = "(" ++ showExpression x ++ " ^ " ++ showExpression y ++ ")"
showExpression (Function "leq" [x,y]) = "(" ++ showExpression x ++ " <= " ++ showExpression y ++ ")"
showExpression (Function "gt" [x,y]) = "(" ++ showExpression x ++ " > " ++ showExpression y ++ ")"
showExpression (Function "geq" [x,y]) = "(" ++ showExpression x ++ " >= " ++ showExpression y ++ ")"
showExpression (Function "not" [(Function "eq" [x,y])]) = "(" ++ showExpression x ++ " != " ++ showExpression y ++ ")"
showExpression (Function "and" pars) = "(" ++ (infixString (map showExpression pars) " & ") ++ ")"
showExpression (Function "or" pars) = "(" ++ (infixString (map showExpression pars) " | ") ++ ")"
showExpression (Function name pars)  = name ++ optionalParentheses (infixString parts ", ")
  where
    parts = map (\x -> if x!!0 == '(' then take (length x - 2) (drop 1 x) else x) (map showExpression pars)

-- These functions allow easy manipulation of triples.
fst3 (a,b,c) = a
snd3 (a,b,c) = b
thd3 (a,b,c) = c

fst4 (a,b,c,d) = a
snd4 (a,b,c,d) = b
thd4 (a,b,c,d) = c
frt4 (a,b,c,d) = d


-- This function takes a list of strings and a separating string
-- and produces one string that consists of all the strings of the list
-- in a row, separated by the separating string.
infixString :: [String] -> String -> String
infixString []     inf = []
infixString [x]    inf = x
infixString (x:xs) inf = x ++ inf ++ (infixString xs inf)

-- This function takes a string and surrounds it by parentheses,
-- except for the case that it is empty.
optionalParentheses :: String -> String
optionalParentheses ""   = ""
optionalParentheses text = "(" ++ text ++ ")"  

-- This function takes a list of pairs of strings [(s1,t1), (s2,t2), ...], 
-- and provides the string (s1 : t1, s2 : t2, ...)
printPairs :: [(String, String)] -> String
printPairs pairs = optionalParentheses (infixString [left ++ " : " ++ right | (left, right) <- pairs] ", ")

-- This function takes a list of items and makes a comma-separated list of it,
-- surrounded by parenthesis.
commaList :: [String] -> String
commaList items = optionalParentheses (infixString items ", ")

-- The function provides the string between a certain begin and end character,
-- and also the remaining string after the end character.
takeBetween :: String -> Char -> Char -> (String, String)
takeBetween s from to = (between, remainder)
  where
    between   = takeWhile (/= to) (drop 1 (dropWhile (/= from) s))
    remainder = drop 1 (dropWhile (/= to) (drop 1 (dropWhile (/= from) s)))
	
-- This function takes a string and a delimiter, and splits the string
-- based on this delimiter.
split :: String -> Char -> [String]
split []     delim              = [""]
split (c:cs) delim | c == delim = "" : rest
                   | otherwise  = (c : head rest) : tail rest
  where
    rest = split cs delim

-- This functions removes the spaces from a string
removeSpaces ""      = ""
removeSpaces (' ': s) = removeSpaces s
removeSpaces (x  :xs)  = x:(removeSpaces xs)

-- This function changes the line breaks of a string to spaces.
removeLineBreaks :: String -> String
removeLineBreaks []                   = []
removeLineBreaks (x:xs) | ('\n' ==) x = ' ' : removeLineBreaks xs
                        | ('\r' ==) x = ' ' : removeLineBreaks xs
						| otherwise   =  x  : removeLineBreaks xs