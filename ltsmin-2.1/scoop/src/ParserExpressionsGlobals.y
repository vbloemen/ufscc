{
module ParserExpressionsGlobals where

import ParserAux
import DataSpec
import Data.Char
import Expressions
import Auxiliary
import Processes

}

%monad { ParserMonad }  { thenParserMonad } { returnParserMonad }

%name parseExpression
%tokentype { Token }
%error { parseError }
%lexer { lexer } { TokenEOF } 

%left '||'
%left '++'
%left '=>'
%left '|'
%left '&'
%left '!'
%nonassoc '=' '!='
%nonassoc '<' '<=' '>=' '>'
%left '+' '-'
%left '*' '/'
%left '.'
%left Negation


%token 
      ','             { TokenComma }
      '('             { TokenOB }
      '|'             { TokenOr }
      ')'             { TokenCB }
      '+'             { TokenPlus }
      '&'             { TokenAnd }
      '!'             { TokenNot }
      '!='            { TokenNotEqual }
      '-'             { TokenMinus }
      '/'             { TokenDivide }
      '*'             { TokenMultiply }
      ':='            { TokenBecomes }
      '<'             { TokenSmaller }
      '>'             { TokenGreater }
      '<='            { TokenSmallerEq }
      '>='            { TokenGreaterEq }
      '='             { TokenEqual }
      string          { TokenString $$ }


%%
Updates      : Update                           { [$1] }
             | Updates ',' Update               { $3 : $1 }
Update       : Expression ':=' Expression           { ($1, $3) }

Expressions : Expression                        { [$1] }
            | Expressions ',' Expression        { $3 : $1 }

Expression : Expression '<' Expression          { Function "lt" [$1, $3] }
           | Expression '<=' Expression         { Function "leq" [$1, $3] }
           | Expression '>' Expression          { Function "gt" [$1, $3] }
           | Expression '&' Expression          { Function "and" [$1, $3] }
           | Expression '|' Expression          { Function "or" [$1, $3] }
           | Expression '>=' Expression         { Function "geq" [$1, $3] }
           | Expression '=' Expression          { Function "eq" [$1, $3] }
           | Expression '!=' Expression         { Function "not" [Function "eq" [$1, $3]] }
           | '!' Expression                     { Function "not" [$2] }
           | Expression '+' Expression          { Function "plus" [$1, $3] }
           | Expression '-' Expression          { Function "minus" [$1, $3] }
    	   | '-' Expression %prec Negation      { Function "multiply" [Variable "-1", $2]  }
           | Expression '*' Expression          { Function "multiply" [$1, $3] }
           | Expression '/' Expression          { Function "divide" [$1, $3] }
           | string '(' Expressions ')'         { Function $1 (reverse $3) }
           | string                             { Variable $1 }
           | '(' Expression ')'                 { $2 } 

{

parseError :: Token -> ParserMonad a 
parseError t = failParserMonad ("Error: Parser did not expect token \"" ++ show t ++ "\"")


data Token = TokenSum
           | TokenSeq  
           | TokenString String
           | TokenIndependence
           | TokenUntilFormula
           | TokenOB 
           | TokenCB 
           | TokenOSB 
           | TokenCSB 
           | TokenOA
           | TokenCA
           | TokenOr
           | TokenImplies
           | TokenColon
           | TokenBool
           | TokenPlus
           | TokenPlusPlus
           | TokenMinus
           | TokenDivide
           | TokenMultiply
           | TokenThen
           | TokenElse
           | TokenActions
           | TokenComma
           | TokenEqual
           | TokenNotEqual
           | TokenNot
           | TokenPSum
           | TokenDotDot
           | TokenHide
           | TokenRename
           | TokenEncap
           | TokenAnd
           | TokenComm
           | TokenConstant
           | TokenType
           | TokenGlobal
           | TokenFunction
           | TokenInit
           | TokenSmaller
           | TokenGreater
           | TokenSmallerEq
           | TokenGreaterEq
           | TokenProbDef
           | TokenBecomes
           | TokenEOF
instance Show Token where
  show x =
   case x of 
     TokenSum -> "sum("
     TokenSeq -> "."
     TokenString s -> s
     TokenIndependence -> "||"
     TokenUntilFormula -> "U"
     TokenOB  -> "("
     TokenCB  -> ")"
     TokenOSB -> "["
     TokenCSB -> "]"
     TokenOA  -> "{"
     TokenCA  -> "}"
     TokenOr -> "|"
     TokenImplies -> "=>"
     TokenColon -> ":"
     TokenBool -> "Bool"
     TokenPlus -> "+"
     TokenPlusPlus -> "++"
     TokenMinus -> "-"
     TokenDivide -> "/"
     TokenMultiply -> "*"
     TokenThen -> "<|"
     TokenElse -> "|>"
     TokenActions -> "actions"
     TokenComma -> ","
     TokenEqual -> "="
     TokenNotEqual -> "!="
     TokenNot -> "!"
     TokenPSum -> "psum"
     TokenDotDot -> ".."
     TokenHide -> "hide"
     TokenGlobal -> "global"
     TokenEncap -> "encap"
     TokenAnd -> "&"
     TokenComm -> "comm"
     TokenConstant -> "constant"
     TokenType -> "type"
     TokenFunction -> "function"
     TokenInit -> "init"
     TokenSmaller -> "<"
     TokenGreater -> ">"
     TokenSmallerEq -> "<="
     TokenGreaterEq -> ">="
     TokenProbDef -> "->"
     TokenBecomes -> ":="
     TokenEOF -> "EOF"

-- lexer :: String -> [Token] 
lexer :: (Token -> ParserMonad a) -> ParserMonad a
lexer cont ('\n':cs) = \line -> lexer cont cs (line + 1)
lexer cont ('\r':cs) = \line -> lexer cont cs line
lexer cont [] = cont TokenEOF [] 
lexer cont ('.':'.':cs) = cont TokenDotDot cs
lexer cont (c:cs) 
      | isSpace c = lexer cont cs 
      | (\x -> isDigit x) c = lexNumber cont (c:cs)
      | (\x -> isAlpha x) c = lexString cont (c:cs)
lexer cont ('.':cs) = cont TokenSeq cs 
lexer cont ('|':'|':cs) = cont TokenIndependence cs
lexer cont ('|':'>':cs) = cont TokenElse cs 
lexer cont ('<':'|':cs) = cont TokenThen cs 
lexer cont ('&':cs) = cont TokenAnd cs 
lexer cont ('<':'=':cs) = cont TokenSmallerEq cs 
lexer cont ('>':'=':cs) = cont TokenGreaterEq cs 
lexer cont ('<':cs) = cont TokenSmaller cs 
lexer cont ('>':cs) = cont TokenGreater cs 
lexer cont ('(':cs) = cont TokenOB cs 
lexer cont ('|':cs) = cont TokenOr cs 
lexer cont (')':cs) = cont TokenCB cs 
lexer cont ('[':cs) = cont TokenOSB cs 
lexer cont (']':cs) = cont TokenCSB cs 
lexer cont ('{':cs) = cont TokenOA cs 
lexer cont ('}':cs) = cont TokenCA cs 
lexer cont ('+':'+':cs) = cont TokenPlusPlus cs
lexer cont ('-':'>':cs) = cont TokenProbDef cs
lexer cont (',':cs) = cont TokenComma cs
lexer cont ('+':cs) = cont TokenPlus cs
lexer cont ('-':cs) = cont TokenMinus cs
lexer cont ('!':'=':cs) = cont TokenNotEqual cs
lexer cont ('!':cs) = cont TokenNot cs
lexer cont ('/':cs) = cont TokenDivide cs
lexer cont ('*':cs) = cont TokenMultiply cs
lexer cont (':':'=':cs) = cont TokenBecomes cs
lexer cont (':':cs) = cont TokenColon cs
lexer cont ('=':'>':cs) = cont TokenImplies cs
lexer cont ('=':cs) = cont TokenEqual cs
lexer cont s = \line -> error("Error: Lexer could not deal with character \"" ++ [head s] ++ "\" on line " ++ show line)

lexString cont cs = cont (TokenString var) rest
  where
    (var,rest) = getString cs
lexNumber cont cs = cont (TokenString var) rest
  where
    (var,rest) = getNumber cs

getString [] = ("","")
getString ('.':'.':cs) = ("", '.':'.':cs)
getString (c:cs) | elem c "\n\t\r ',()&=*.<>+{}^:-!?/|\"[]" = ("", c:cs)
                 | otherwise = (str,rest)
  where
    (s,r) = getString cs
    str   = c:s
    rest  = r

getNumber [] = ("","")
getNumber ('.':'.':cs) = ("", '.':'.':cs)
getNumber (c:cs) | elem c "\n\t\r ',()&=*<>+_{}:-!?;/|\"[]" = ("", c:cs)
                 | otherwise = (str,rest)
  where
    (s,r) = getNumber cs
    str   = c:s
    rest  = r

} 
