{
module ParserUntil where

import Data.Char
import Processes

import ParserAux

}

%name parseUntil
%tokentype { Token }
%error { parseError }

%nonassoc 'U'
%left '|'
%left '!'

%token 
      '('             { TokenOB }
      ')'             { TokenCB }
      'U'             { TokenUntil }
      '|'             { TokenOr }
      ','             { TokenComma }
      '!'             { TokenNot }
      string          { TokenString $$ }
      true            { TokenTrue }
%%

UntilFormula : ActionExpression 'U' ActionExpression { UntilFormula $1 $3 }

ActionExpression : true                                  { ActionTrue }
                 | string                                { ActionName $1 [] }
                 | string '(' ActionPars ')'             { ActionName $1 $3 }
                 | '(' ActionExpression ')'              { $2 }
                 | '!' ActionExpression                  { ActionNot $2 }
                 | ActionExpression '|' ActionExpression { ActionOr $1 $3 }

ActionPars : string                                      { [$1] }
           | string ',' ActionPars                       { $1 : $3 }

{

parseError :: [Token] -> a 
parseError t = error ("Error: Parser did not expect token \"" ++ show (t!!0) ++ "\" in Until formula")


data Token = TokenString String
           | TokenOB 
           | TokenCB 
           | TokenNot
           | TokenComma
           | TokenOr
           | TokenTrue
           | TokenUntil
instance Show Token where
  show x =
   case x of 
     TokenOB -> "("
     TokenCB -> ")"
     TokenNot -> "!"
     TokenComma -> ","
     TokenOr -> "|"
     TokenTrue -> "true"
     TokenUntil -> "U"

lexerUntil :: String -> [Token] 
lexerUntil [] = [] 
lexerUntil ('t':'r':'u':'e':cs) = TokenTrue : lexerUntil cs
lexerUntil ('U':cs) = TokenUntil : lexerUntil cs
lexerUntil (c:cs) 
      | isSpace c = lexerUntil cs 
      | (\x -> isDigit x || isAlpha x) c = lexString (c:cs)
lexerUntil ('|':cs) = TokenOr : lexerUntil cs
lexerUntil ('(':cs) = TokenOB : lexerUntil cs 
lexerUntil (',':cs) = TokenComma : lexerUntil cs 
lexerUntil (')':cs) = TokenCB : lexerUntil cs 
lexerUntil ('!':cs) = TokenNot : lexerUntil cs
lexerUntil s = error("Error: Lexer could not deal with character " ++ s ++ " in Until formula")

lexString cs = TokenString var : lexerUntil rest
  where
    (var,rest) = getString cs

getString [] = ("","")
getString ('.':'.':cs) = ("", '.':'.':cs)
getString (c:cs) | elem c " ',()&=.<>+{}:*!?;/|\"[]" = ("", c:cs)
                 | otherwise = (str,rest)
  where
    (s,r) = getString cs
    str   = c:s
    rest  = r

}