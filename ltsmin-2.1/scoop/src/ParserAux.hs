module ParserAux where

import DataSpec
import Data.Char
import Expressions
import Auxiliary
import Processes

printExpressions [] = []
printExpressions updates = infixString [(printExpression var) ++ " := " ++ (printExpression val) | (var,val) <- updates] ", "

data Item = DataActions [(String, ActionType)]
          | DataHiding [String] 
          | DataParam [String] 
          | DataGlobal (Variable, Type) Expression 
          | DataRenaming [(String, String)] 
          | DataUntilFormula String
          | DataFormula Expression
          | DataEncapsulation [String]   
          | DataNoCommunication [String]   
          | DataReach [String]   
          | DataReachCondition Expression
          | DataStateReward Expression Expression
          | DataCommunication [(String, String, String)] 
          | DataConstant String Expression
          | DataEnumType String [String] 
          | DataNat String
          | DataQueue String
          | DataRangeType String Expression Expression
          | DataFunction String [([String], String)]
          | ParserProcess Process
          | ParserInitialProcess InitialProcessDefinition
              deriving (Show, Eq)

data ActionType = None | Bool | IntRange Expression Expression deriving (Show, Eq)

data InitialProcessDefinition = InitSingleProcess InitialProcess
                              | InitParallel InitialProcessDefinition InitialProcessDefinition
                              | InitHiding [Action] InitialProcessDefinition
                              | InitEncapsulation [Action] InitialProcessDefinition
                              | InitRenaming [(Action, Action)] InitialProcessDefinition
                                   deriving (Show, Eq)

type LineNumber = Int
data ParseResult a = Ok a | Failed String
type ParserMonad a = String -> LineNumber -> ParseResult a

getLineNo :: ParserMonad LineNumber
getLineNo = \s l -> Ok l


thenParserMonad :: ParserMonad a -> (a -> ParserMonad b) -> ParserMonad b
m `thenParserMonad ` k = \s -> \l ->
   case (m s l) of 
       Ok a -> k a s l
       Failed e -> Failed e

returnParserMonad :: a -> ParserMonad a
returnParserMonad a = \s -> \l -> Ok a

failParserMonad :: String -> ParserMonad a
failParserMonad err = \s -> \l -> Failed (err ++ " on line " ++ show l)

-- For ParserUntil:
data UntilFormula     = UntilFormula ActionExpression ActionExpression  deriving (Show, Eq)
data ActionExpression = ActionName String [String]
                      | ActionOr   ActionExpression ActionExpression
                      | ActionNot  ActionExpression
                      | ActionTrue
                      deriving (Show, Eq)
