{
module Parser where

import ParserAux
import Data.Char
import Auxiliary
import DataSpec
import Expressions
import Processes

}

%monad { ParserMonad }  { thenParserMonad } { returnParserMonad }

%name parse
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
%left '^'
%left '.'
%left Negation


%token 
      Sum             { TokenSum }
      PSum            { TokenPSum }
      ','             { TokenComma }
      '.'             { TokenSeq }
      '||'            { TokenIndependence }
      '['             { TokenOSB }
      ']'             { TokenCSB }
      '('             { TokenOB }
      '|'             { TokenOr }
      '{'             { TokenOA }
      '}'             { TokenCA }
      ')'             { TokenCB }
      '+'             { TokenPlus }
      '^'             { TokenPower }
      '++'            { TokenPlusPlus }
      '&'             { TokenAnd }
      '!'             { TokenNot }
      '!='            { TokenNotEqual }
      '-'             { TokenMinus }
      '/'             { TokenDivide }
      '*'             { TokenMultiply }
      ':'             { TokenColon}
      ':='            { TokenBecomes }
      '<'             { TokenSmaller }
      '>'             { TokenGreater }
      '..'            { TokenDotDot }
      '@'             { TokenAt }
      '<='            { TokenSmallerEq }
      init            { TokenInit }
      '>='            { TokenGreaterEq }
      '='             { TokenEqual }
      '=>'            { TokenImplies }
      '->'            { TokenProbDef }
      true            { TokenTrue }
      false           { TokenFalse }
      string          { TokenString $$ }
      hide            { TokenHide }
      param           { TokenParam }
      rename          { TokenRename }
      bool            { TokenBool }
      actions         { TokenActions }
      encap           { TokenEncap }
      comm            { TokenComm }
      nocomm          { TokenNoComm }
      reach           { TokenReach }
      reachCondition  { TokenReachCondition }
      stateReward     { TokenStateReward }
      constant        { TokenConstant }
      formula         { TokenFormula }
      global          { TokenGlobal }
      until           { TokenUntilFormula }
      type            { TokenType }
      function        { TokenFunction }
      Nat             { TokenNat }
      Queue           { TokenQueue }


%%

Specification : Items { $1 }

Items : {- empty -}             { [] }
      | Item Items              { $1 : $2 }

Item : hide     Strings                                        { DataHiding $2 }
     | param    Strings                                        { DataParam $2 }
     | rename   StringPairs                                    { DataRenaming $2 }
     | actions  ActionTypes                                    { DataActions $2 }
     | global   string ':' Type '=' Expression                 { DataGlobal ($2, $4) $6 }
     | encap    Strings                                        { DataEncapsulation $2 }
     | comm     CommActions                                    { DataCommunication $2 }
     | nocomm   Strings                                        { DataNoCommunication $2 }
     | reach    StringsWithPars                                { DataReach $2 }
     | reachCondition Expression                               { DataReachCondition $2 }
     | stateReward Expression '->' Expression                  { DataStateReward $2 $4 }
     | until    '[' String2 ']'                                { DataUntilFormula $3 }
     | formula  Expression                                     { DataFormula $2 }
     | constant string '=' Expression                          { DataConstant $2 $4 }
     | type     string '=' '(' Strings ')'                     { DataEnumType $2 $5 }
     | type     string '=' '{' Expression '..'  Expression '}' { DataRangeType $2 $5 $7 }
--     | type     string '=' Nat                                 { DataNat $2 }
--     | type     string '=' Queue                               { DataQueue $2 }
     | function string '=' '(' FunctionDefinition ')'          { DataFunction $2 $5 }
     | init     InitialProcess                                 { ParserInitialProcess $2 }
     | string   '(' StatePars ')' '=' RHS                      { ParserProcess (Process $1 (reverse $3) $6) }
     | string '=' RHS                                          { ParserProcess (Process $1 [] $3) }

String2 :                                                      { "" }
        | string String2                                       { $1 ++ " " ++ $2 }
        | '|' String2                                          { "|" ++ " " ++ $2 }
        | '!' String2                                          { "!" ++ " " ++ $2 }
        | '(' String2                                          { "(" ++ " " ++ $2 }
        | ')' String2                                          { ")" ++ " " ++ $2 }
        | ',' String2                                          { "," ++ " " ++ $2 }



ActionTypes : ActionType                                       { [$1] }
            | ActionType ',' ActionTypes                       { $1:$3 }

ActionType : string                                            { ($1, None) }
           | string ':' bool                                   { ($1, Bool) }
           | string ':' '{' Expression '..' Expression '}'     { ($1, IntRange $4 $6) }


FunctionDefinition : Mapping                        { [$1] }
                   | Mapping ',' FunctionDefinition { $1:$3 }

Mapping : From '->' string { ($1, $3) }

From : string           { [$1] }
     | string '*' From  { $1:$3 }

CommActions : CommAction            { [$1] }
            | CommAction ',' CommActions { $1 : $3 }

CommAction : '(' string ',' string ',' string ')' { ($2,$4,$6) }

StringPairs : StringPair                           { [$1] }
            | StringPair ',' StringPairs           { $1 : $3 }

StringPair  : '(' string ',' string ')' { ($2,$4) }

Strings : string                 { [$1] }
        | string ',' Strings  { $1 : $3 }

StringsWithPars : StringOrPar                 { [$1] }
                | StringOrPar ',' StringsWithPars  { $1 : $3 }

StringOrPar : string { $1 }
            | string '(' Strings ')' {$1 ++ commaList $3}
            | '<' StringOrPar '>' {"rate(" ++ $2 ++ ")"}

InitialProcess : string OptionalParametersSq                      { InitSingleProcess (InitialProcess $1 $2 []) }
               | string '(' Constants ')' OptionalParametersSq    { InitSingleProcess (InitialProcess $1 $5 $3) }
			   | InitialProcess '||' InitialProcess               { InitParallel $1 $3 }
               | hide '(' Strings ':' InitialProcess ')'          { InitHiding $3 $5 }
               | encap '(' Strings ':' InitialProcess ')'         { InitEncapsulation $3 $5 }
               | rename '(' StringPairs ':' InitialProcess ')'    { InitRenaming $3 $5 }
               | '(' InitialProcess ')'                           { $2 }


OptionalParameters : {- empty -}         { [] }
                   | '(' ')'             { [] }
                   | '(' Expressions ')' { reverse $2 }

OptionalParametersSq : {- empty -}         { [] }
                     | '[' ']'             { [] }
                     | '[' Expressions ']' { reverse $2 }

Reward : '@' Expression { $2 }

RHS : RHS '++' RHS                                                                        { Plus ([$1] ++ [$3]) }
    | string OptionalParameters Reward '.' RHS                                              { if (take 4 $1) /= "rate" then ActionPrefix $3 $1 $2 [("i0", TypeRange 1 1, (Variable "1"))] $5 else error("Error: you cannot use action names starting with 'rate'")}
    | string OptionalParameters        '.' RHS                                              { if (take 4 $1) /= "rate" then ActionPrefix (Variable "0") $1 $2 [("i0", TypeRange 1 1, (Variable "1"))] $4 else error("Error: you cannot use action names starting with 'rate'")}
    | string OptionalParameters '{' UpdateGlobals '}' Reward '.' RHS                        { if (take 4 $1) /= "rate" then ActionPrefix $6 ($1  ++ "{" ++ (printExpressions $4) ++ "}") $2 [("i0", TypeRange 1 1, (Variable "1"))] $8 else error("Error: you cannot use action names starting with 'rate'")}
    | string OptionalParameters '{' UpdateGlobals '}'        '.' RHS                        { if (take 4 $1) /= "rate" then ActionPrefix (Variable "0") ($1  ++ "{" ++ (printExpressions $4) ++ "}") $2 [("i0", TypeRange 1 1, (Variable "1"))] $7 else error("Error: you cannot use action names starting with 'rate'")}
    | string OptionalParameters Reward '.' PSum '(' IndepProbs ':' RHS ')'                  { if (take 4 $1) /= "rate" then ActionPrefix $3 $1 $2 $7 $9 else error("Error: you cannot use action names starting with 'rate'")}
    | string OptionalParameters        '.' PSum '(' IndepProbs ':' RHS ')'                  { if (take 4 $1) /= "rate" then ActionPrefix (Variable "0") $1 $2 $6 $8 else error("Error: you cannot use action names starting with 'rate'")}
    | string OptionalParameters '{' UpdateGlobals '}' Reward '.' PSum '(' IndepProbs ':' RHS ')'   {if (take 4 $1) /= "rate" then  ActionPrefix $6 ($1  ++ "{" ++ (printExpressions $4) ++ "}") $2 $10 $12 else error("Error: you cannot use action names starting with 'rate'")}
    | string OptionalParameters '{' UpdateGlobals '}'        '.' PSum '(' IndepProbs ':' RHS ')'   {if (take 4 $1) /= "rate" then  ActionPrefix (Variable "0") ($1  ++ "{" ++ (printExpressions $4) ++ "}") $2 $9 $11 else error("Error: you cannot use action names starting with 'rate'")}
    | string OptionalParameters Reward '.' PSum '(' IndepProbs ':' '{' UpdateGlobals '}' RHS ')'   {if (take 4 $1) /= "rate" then  ActionPrefix $3 ($1  ++ "{" ++ (printExpressions $10) ++ "}") $2 $7 $12 else error("Error: you cannot use action names starting with 'rate'")}
    | string OptionalParameters        '.' PSum '(' IndepProbs ':' '{' UpdateGlobals '}' RHS ')'   {if (take 4 $1) /= "rate" then  ActionPrefix (Variable "0") ($1  ++ "{" ++ (printExpressions $9) ++ "}") $2 $6 $11 else error("Error: you cannot use action names starting with 'rate'")}
    | string OptionalParameters Reward '.' PSum '(' Probs ')'                {if (take 4 $1) /= "rate" then  ActionPrefix2 $3 $1 $2 $7 else error("Error: you cannot use action names starting with 'rate'")}
    | string OptionalParameters        '.' PSum '(' Probs ')'                {if (take 4 $1) /= "rate" then  ActionPrefix2 (Variable "0") $1 $2 $6 else error("Error: you cannot use action names starting with 'rate'")}
    | string OptionalParameters '{' UpdateGlobals '}' Reward '.' PSum '(' Probs ')'                {if (take 4 $1) /= "rate" then  ActionPrefix2 $6 ($1  ++ "{" ++ (printExpressions $4) ++ "}") $2 $10 else error("Error: you cannot use action names starting with 'rate'")}
    | string OptionalParameters '{' UpdateGlobals '}'        '.' PSum '(' Probs ')'                {if (take 4 $1) /= "rate" then  ActionPrefix2 (Variable "0") ($1  ++ "{" ++ (printExpressions $4) ++ "}") $2 $9 else error("Error: you cannot use action names starting with 'rate'")}
    | Sum '(' string ':' Type ',' RHS ')'                             { Sum $3 $5 $7 }
    | Expression '=>' RHS                                             { Implication $1 $3 }
    | ProcessInstantiation                                            { $1 }
    | '(' RHS ')'                                                     { $2 }
    | '<' Expression '>' '.' RHS                                      { LambdaPrefix (Variable "0") $2 $5}
    | '<' Expression '>' Reward '.' RHS                               { LambdaPrefix $4 $2 $6}

ProcessInstantiation : string '[' Expressions ']'                     { ProcessInstance $1 (reverse $3) }
                     | string '[' Updates ']'                         { ProcessInstance2 $1 (reverse $3) }
                     | string '[' ']'                                 { ProcessInstance2 $1 [] }


Type         : string                                { TypeName $1 }
             | Queue                                 { TypeName "Queue" }
             | Nat                                   { TypeName "Nat" }
             | '{' Expression '..' Expression '}'    { TypeRangeExpressions $2 $4 }

IndepProbs   : IndepProb                        { [$1] }
             | IndepProb '||' IndepProbs        { $1 : $3 }
IndepProb    : string ':' Type ',' Expression   { ($1, $3, $5) }
      
StatePars    : StatePar                         { [$1] }
             | StatePars ',' StatePar           { $3 : $1 }
StatePar     : string ':' Type { ($1, $3) }

Updates      : Update                           { [$1] }
             | Updates ',' Update               { $3 : $1 }
Update       : string ':=' Expression       { ($1, $3) }

Constants    : ConstantDef                      { [$1] }
             | Constants ',' ConstantDef        { $3 : $1 }
ConstantDef  : string '=' string                { ($1, $3) }

UpdateGlobals      : UpdateGlobal                     { [$1] }
                   | UpdateGlobals ',' UpdateGlobal   { $3 : $1 }
UpdateGlobal       : Expression ':=' Expression       { ($1, $3) }

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
           | Expression '^' Expression          { Function "power" [$1, $3] }
           | Expression '-' Expression          { Function "minus" [$1, $3] }
    	   | '-' Expression %prec Negation      { Function "multiply" [Variable "-1", $2]  }
           | Expression '*' Expression          { Function "multiply" [$1, $3] }
           | Expression '/' Expression          { Function "divide" [$1, $3] }
           | string '(' Expressions ')'         { Function $1 (reverse $3) }
           | string                             { Variable $1 }
           | '(' Expression ')'                 { $2 } 

Probs      : Expression '->' RHS                { [($1, $3)]} 
           | Expression '->' RHS '++' Probs      { ($1, $3) : $5 }
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
           | TokenPower
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
           | TokenAt
           | TokenHide
           | TokenParam
           | TokenTrue
           | TokenFalse
           | TokenRename
           | TokenEncap
           | TokenAnd
           | TokenComm
           | TokenNoComm
           | TokenReach
           | TokenReachCondition
           | TokenStateReward
           | TokenConstant
           | TokenFormula
           | TokenType
           | TokenGlobal
           | TokenFunction
           | TokenNat
           | TokenQueue
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
     TokenPower -> "^"
     TokenPlusPlus -> "++"
     TokenMinus -> "-"
     TokenDivide -> "/"
     TokenMultiply -> "*"
     TokenThen -> "<|"
     TokenAt -> "@"
     TokenElse -> "|>"
     TokenActions -> "actions"
     TokenComma -> ","
     TokenEqual -> "="
     TokenTrue -> "true"
     TokenFalse -> "false"
     TokenNotEqual -> "!="
     TokenNot -> "!"
     TokenPSum -> "psum"
     TokenDotDot -> ".."
     TokenHide -> "hide"
     TokenParam -> "param"
     TokenGlobal -> "global"
     TokenEncap -> "encap"
     TokenAnd -> "&"
     TokenComm -> "comm"
     TokenNoComm -> "nocomm"
     TokenReach -> "reach"
     TokenReachCondition -> "reachCondition"
     TokenStateReward -> "stateReward"
     TokenConstant -> "constant"
     TokenFormula -> "formula"
     TokenType -> "type"
     TokenFunction -> "function"
     TokenQueue -> "Queue"
     TokenNat -> "Nat"
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
lexer cont ('-':'-':cs) = lexComment cont (cs)
lexer cont ('.':'.':cs) = cont TokenDotDot cs
lexer cont ('i':'n':'i':'t':' ':cs) = cont TokenInit cs
lexer cont ('i':'n':'i':'t':'\n':cs) = cont TokenInit ('\n':cs)
lexer cont ('i':'n':'i':'t':'\r':cs) = cont TokenInit ('\r':cs)
lexer cont ('i':'n':'i':'t':'\t':cs) = cont TokenInit ('\t':cs)
lexer cont ('u':'n':'t':'i':'l':' ':cs) = cont TokenUntilFormula cs
lexer cont ('u':'n':'t':'i':'l':'\n':cs) = cont TokenUntilFormula ('\n':cs)
lexer cont ('u':'n':'t':'i':'l':'\r':cs) = cont TokenUntilFormula ('\r':cs)
lexer cont ('u':'n':'t':'i':'l':'\t':cs) = cont TokenUntilFormula ('\t':cs)
lexer cont ('b':'o':'o':'l':' ':cs) = cont TokenBool cs
lexer cont ('b':'o':'o':'l':'\n':cs) = cont TokenBool ('\n':cs)
lexer cont ('b':'o':'o':'l':'\r':cs) = cont TokenBool ('\r':cs)
lexer cont ('b':'o':'o':'l':'\t':cs) = cont TokenBool ('\t':cs)
lexer cont ('o':'b':'s':'e':'r':'v':'e':' ':cs) = cont TokenActions cs
lexer cont ('o':'b':'s':'e':'r':'v':'e':'\n':cs) = cont TokenActions ('\n':cs)
lexer cont ('o':'b':'s':'e':'r':'v':'e':'\r':cs) = cont TokenActions ('\r':cs)
lexer cont ('o':'b':'s':'e':'r':'v':'e':'\t':cs) = cont TokenActions ('\t':cs)
lexer cont ('h':'i':'d':'e':' ':cs) = cont TokenHide cs
lexer cont ('h':'i':'d':'e':'\n':cs) = cont TokenHide ('\n':cs)
lexer cont ('h':'i':'d':'e':'\r':cs) = cont TokenHide ('\r':cs)
lexer cont ('h':'i':'d':'e':'\t':cs) = cont TokenHide ('\t':cs)
lexer cont ('h':'i':'d':'e':'(':cs) = cont TokenHide ('(':cs)

lexer cont ('p':'a':'r':'a':'m':' ':cs) = cont TokenParam cs
lexer cont ('p':'a':'r':'a':'m':'\n':cs) = cont TokenParam ('\n':cs)
lexer cont ('p':'a':'r':'a':'m':'\r':cs) = cont TokenParam ('\r':cs)
lexer cont ('p':'a':'r':'a':'m':'\t':cs) = cont TokenParam ('\t':cs)
lexer cont ('p':'a':'r':'a':'m':'(':cs) = cont TokenParam ('(':cs)

lexer cont ('t':'r':'u':'e':' ':cs) = error ("Please use \"T\" instead of \"true\"")
lexer cont ('t':'r':'u':'e':'\n':cs) = error ("Please use \"T\" instead of \"true\"")
lexer cont ('t':'r':'u':'e':'\r':cs) = error ("Please use \"T\" instead of \"true\"")
lexer cont ('t':'r':'u':'e':'\t':cs) = error ("Please use \"T\" instead of \"true\"")
lexer cont ('t':'r':'u':'e':'(':cs) = error ("Please use \"T\" instead of \"true\"")

lexer cont ('f':'a':'l':'s':'e':' ':cs) = error ("Please use \"F\" instead of \"false\"")
lexer cont ('f':'a':'l':'s':'e':'\n':cs) = error ("Please use \"F\" instead of \"false\"")
lexer cont ('f':'a':'l':'s':'e':'\r':cs) = error ("Please use \"F\" instead of \"false\"")
lexer cont ('f':'a':'l':'s':'e':'\t':cs) = error ("Please use \"F\" instead of \"false\"")
lexer cont ('f':'a':'l':'s':'e':'(':cs) = error ("Please use \"F\" instead of \"false\"")


lexer cont ('g':'l':'o':'b':'a':'l':' ':cs) = cont TokenGlobal cs
lexer cont ('g':'l':'o':'b':'a':'l':'\n':cs) = cont TokenGlobal ('\n':cs)
lexer cont ('g':'l':'o':'b':'a':'l':'\r':cs) = cont TokenGlobal ('\r':cs)
lexer cont ('g':'l':'o':'b':'a':'l':'\t':cs) = cont TokenGlobal ('\t':cs)
lexer cont ('g':'l':'o':'b':'a':'l':'(':cs) = cont TokenGlobal ('(':cs)

lexer cont ('s':'u':'m':cs) | not (isAlpha (head cs)) = cont TokenSum cs
lexer cont ('p':'s':'u':'m':cs) | not (isAlpha (head cs)) = cont TokenPSum cs
lexer cont ('c':'o':'n':'s':'t':'a':'n':'t':' ':cs) = cont TokenConstant cs
lexer cont ('c':'o':'n':'s':'t':'a':'n':'t':'\n':cs) = cont TokenConstant ('\n':cs)
lexer cont ('c':'o':'n':'s':'t':'a':'n':'t':'\r':cs) = cont TokenConstant ('\r':cs)
lexer cont ('c':'o':'n':'s':'t':'a':'n':'t':'\t':cs) = cont TokenConstant ('\t':cs)
lexer cont ('f':'o':'r':'m':'u':'l':'a':' ':cs) = cont TokenFormula cs
lexer cont ('f':'o':'r':'m':'u':'l':'a':'\n':cs) = cont TokenFormula ('\n':cs)
lexer cont ('f':'o':'r':'m':'u':'l':'a':'\r':cs) = cont TokenFormula ('\r':cs)
lexer cont ('f':'o':'r':'m':'u':'l':'a':'\t':cs) = cont TokenFormula ('\t':cs)
lexer cont ('t':'y':'p':'e':' ':cs) = cont TokenType cs
lexer cont ('t':'y':'p':'e':'\n':cs) = cont TokenType ('\n':cs)
lexer cont ('t':'y':'p':'e':'\r':cs) = cont TokenType ('\r':cs)
lexer cont ('t':'y':'p':'e':'\t':cs) = cont TokenType ('\t':cs)
lexer cont ('f':'u':'n':'c':'t':'i':'o':'n':' ':cs) = cont TokenFunction cs
lexer cont ('f':'u':'n':'c':'t':'i':'o':'n':'\n':cs) = cont TokenFunction ('\n':cs)
lexer cont ('f':'u':'n':'c':'t':'i':'o':'n':'\r':cs) = cont TokenFunction ('\r':cs)
lexer cont ('f':'u':'n':'c':'t':'i':'o':'n':'\t':cs) = cont TokenFunction ('\t':cs)

lexer cont ('Q':'u':'e':'u':'e':' ':cs) = cont TokenQueue cs
lexer cont ('Q':'u':'e':'u':'e':'\n':cs) = cont TokenQueue ('\n':cs)
lexer cont ('Q':'u':'e':'u':'e':'\r':cs) = cont TokenQueue ('\r':cs)
lexer cont ('Q':'u':'e':'u':'e':'\t':cs) = cont TokenQueue ('\t':cs)

lexer cont ('N':'a':'t':' ':cs) = cont TokenNat cs
lexer cont ('N':'a':'t':'\n':cs) = cont TokenNat ('\n':cs)
lexer cont ('N':'a':'t':'\r':cs) = cont TokenNat ('\r':cs)
lexer cont ('N':'a':'t':'\t':cs) = cont TokenNat ('\t':cs)


lexer cont ('e':'n':'c':'a':'p':' ':cs) = cont TokenEncap cs
lexer cont ('e':'n':'c':'a':'p':'\n':cs) = cont TokenEncap ('\n':cs)
lexer cont ('e':'n':'c':'a':'p':'\r':cs) = cont TokenEncap ('\r':cs)
lexer cont ('e':'n':'c':'a':'p':'\t':cs) = cont TokenEncap ('\t':cs)
lexer cont ('e':'n':'c':'a':'p':'(':cs) = cont TokenEncap ('(':cs)
lexer cont ('r':'e':'n':'a':'m':'e':' ':cs) = cont TokenRename cs
lexer cont ('r':'e':'n':'a':'m':'e':'\n':cs) = cont TokenRename ('\n':cs)
lexer cont ('r':'e':'n':'a':'m':'e':'\r':cs) = cont TokenRename ('\r':cs)
lexer cont ('r':'e':'n':'a':'m':'e':'\t':cs) = cont TokenRename ('\t':cs)
lexer cont ('r':'e':'n':'a':'m':'e':'(':cs) = cont TokenRename ('(':cs)
lexer cont ('n':'o':'c':'o':'m':'m':' ':cs) = cont TokenNoComm cs
lexer cont ('n':'o':'c':'o':'m':'m':'\n':cs) = cont TokenNoComm ('\n':cs)
lexer cont ('n':'o':'c':'o':'m':'m':'\r':cs) = cont TokenNoComm ('\r':cs)
lexer cont ('n':'o':'c':'o':'m':'m':'\t':cs) = cont TokenNoComm ('\t':cs)
lexer cont ('r':'e':'a':'c':'h':' ':cs) = cont TokenReach cs
lexer cont ('r':'e':'a':'c':'h':'\n':cs) = cont TokenReach ('\n':cs)
lexer cont ('r':'e':'a':'c':'h':'\r':cs) = cont TokenReach ('\r':cs)
lexer cont ('r':'e':'a':'c':'h':'\t':cs) = cont TokenReach ('\t':cs)
lexer cont ('r':'e':'a':'c':'h':'C':'o':'n':'d':'i':'t':'i':'o':'n':' ':cs) = cont TokenReachCondition cs
lexer cont ('r':'e':'a':'c':'h':'C':'o':'n':'d':'i':'t':'i':'o':'n':'\n':cs) = cont TokenReachCondition ('\n':cs)
lexer cont ('r':'e':'a':'c':'h':'C':'o':'n':'d':'i':'t':'i':'o':'n':'\r':cs) = cont TokenReachCondition ('\r':cs)
lexer cont ('r':'e':'a':'c':'h':'C':'o':'n':'d':'i':'t':'i':'o':'n':' ':cs) = cont TokenReachCondition cs
lexer cont ('s':'t':'a':'t':'e':'R':'e':'w':'a':'r':'d':' ':cs) = cont TokenStateReward cs
lexer cont ('s':'t':'a':'t':'e':'R':'e':'w':'a':'r':'d':'\n':cs) = cont TokenStateReward ('\n':cs)
lexer cont ('s':'t':'a':'t':'e':'R':'e':'w':'a':'r':'d':'\r':cs) = cont TokenStateReward ('\r':cs)
lexer cont ('s':'t':'a':'t':'e':'R':'e':'w':'a':'r':'d':' ':cs) = cont TokenStateReward cs
lexer cont ('c':'o':'m':'m':' ':cs) = cont TokenComm cs
lexer cont ('c':'o':'m':'m':'\n':cs) = cont TokenComm ('\n':cs)
lexer cont ('c':'o':'m':'m':'\r':cs) = cont TokenComm ('\r':cs)
lexer cont ('c':'o':'m':'m':'\t':cs) = cont TokenComm ('\t':cs)
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
lexer cont ('^':cs) = cont TokenPower cs
lexer cont ('@':cs) = cont TokenAt cs
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

lexComment cont cs = lexer cont rest
  where
    rest = dropWhile (\x -> x /= '\n' && x /= '\r') cs
lexString cont cs = cont (TokenString var) rest
  where
    (var,rest) = getString cs
lexNumber cont cs = cont (TokenString var) rest
  where
    (var,rest) = getNumber cs

getString [] = ("","")
getString ('.':'.':cs) = ("", '.':'.':cs)
getString (c:cs) | elem c "\n\t\r ',()&=*.<>@+{}^:-/|\"[]" = ("", c:cs)
                 | otherwise = (str,rest)
  where
    (s,r) = getString cs
    str   = c:s
    rest  = r

getNumber [] = ("","")
getNumber ('.':'.':cs) = ("", '.':'.':cs)
getNumber (c:cs) | elem c "\n\t\r ',()&=*<>+_@{}:^-!?/|\"[]" = ("", c:cs)
                 | otherwise = (str,rest)
  where
    (s,r) = getNumber cs
    str   = c:s
    rest  = r

} 
