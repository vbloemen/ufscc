{-# OPTIONS_GHC -w #-}
module ParserExpressionsGlobals where

import ParserAux
import DataSpec
import Data.Char
import Expressions
import Auxiliary
import Processes

-- parser produced by Happy Version 1.19.0

data HappyAbsSyn t4 t5 t6 t7
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7

action_0 (9) = happyShift action_4
action_0 (14) = happyShift action_5
action_0 (16) = happyShift action_6
action_0 (25) = happyShift action_7
action_0 (4) = happyGoto action_8
action_0 (5) = happyGoto action_2
action_0 (7) = happyGoto action_3
action_0 _ = happyFail

action_1 (9) = happyShift action_4
action_1 (14) = happyShift action_5
action_1 (16) = happyShift action_6
action_1 (25) = happyShift action_7
action_1 (5) = happyGoto action_2
action_1 (7) = happyGoto action_3
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (10) = happyShift action_14
action_3 (12) = happyShift action_15
action_3 (13) = happyShift action_16
action_3 (15) = happyShift action_17
action_3 (16) = happyShift action_18
action_3 (17) = happyShift action_19
action_3 (18) = happyShift action_20
action_3 (19) = happyShift action_21
action_3 (20) = happyShift action_22
action_3 (21) = happyShift action_23
action_3 (22) = happyShift action_24
action_3 (23) = happyShift action_25
action_3 (24) = happyShift action_26
action_3 _ = happyFail

action_4 (9) = happyShift action_4
action_4 (14) = happyShift action_5
action_4 (16) = happyShift action_6
action_4 (25) = happyShift action_7
action_4 (7) = happyGoto action_13
action_4 _ = happyFail

action_5 (9) = happyShift action_4
action_5 (14) = happyShift action_5
action_5 (16) = happyShift action_6
action_5 (25) = happyShift action_7
action_5 (7) = happyGoto action_12
action_5 _ = happyFail

action_6 (9) = happyShift action_4
action_6 (14) = happyShift action_5
action_6 (16) = happyShift action_6
action_6 (25) = happyShift action_7
action_6 (7) = happyGoto action_11
action_6 _ = happyFail

action_7 (9) = happyShift action_10
action_7 _ = happyReduce_21

action_8 (8) = happyShift action_9
action_8 (26) = happyAccept
action_8 _ = happyFail

action_9 (9) = happyShift action_4
action_9 (14) = happyShift action_5
action_9 (16) = happyShift action_6
action_9 (25) = happyShift action_7
action_9 (5) = happyGoto action_43
action_9 (7) = happyGoto action_3
action_9 _ = happyFail

action_10 (9) = happyShift action_4
action_10 (14) = happyShift action_5
action_10 (16) = happyShift action_6
action_10 (25) = happyShift action_7
action_10 (6) = happyGoto action_41
action_10 (7) = happyGoto action_42
action_10 _ = happyFail

action_11 _ = happyReduce_17

action_12 (12) = happyShift action_15
action_12 (15) = happyShift action_17
action_12 (16) = happyShift action_18
action_12 (17) = happyShift action_19
action_12 (18) = happyShift action_20
action_12 (20) = happyShift action_22
action_12 (21) = happyShift action_23
action_12 (22) = happyShift action_24
action_12 (23) = happyShift action_25
action_12 (24) = happyShift action_26
action_12 _ = happyReduce_14

action_13 (10) = happyShift action_14
action_13 (11) = happyShift action_40
action_13 (12) = happyShift action_15
action_13 (13) = happyShift action_16
action_13 (15) = happyShift action_17
action_13 (16) = happyShift action_18
action_13 (17) = happyShift action_19
action_13 (18) = happyShift action_20
action_13 (20) = happyShift action_22
action_13 (21) = happyShift action_23
action_13 (22) = happyShift action_24
action_13 (23) = happyShift action_25
action_13 (24) = happyShift action_26
action_13 _ = happyFail

action_14 (9) = happyShift action_4
action_14 (14) = happyShift action_5
action_14 (16) = happyShift action_6
action_14 (25) = happyShift action_7
action_14 (7) = happyGoto action_39
action_14 _ = happyFail

action_15 (9) = happyShift action_4
action_15 (14) = happyShift action_5
action_15 (16) = happyShift action_6
action_15 (25) = happyShift action_7
action_15 (7) = happyGoto action_38
action_15 _ = happyFail

action_16 (9) = happyShift action_4
action_16 (14) = happyShift action_5
action_16 (16) = happyShift action_6
action_16 (25) = happyShift action_7
action_16 (7) = happyGoto action_37
action_16 _ = happyFail

action_17 (9) = happyShift action_4
action_17 (14) = happyShift action_5
action_17 (16) = happyShift action_6
action_17 (25) = happyShift action_7
action_17 (7) = happyGoto action_36
action_17 _ = happyFail

action_18 (9) = happyShift action_4
action_18 (14) = happyShift action_5
action_18 (16) = happyShift action_6
action_18 (25) = happyShift action_7
action_18 (7) = happyGoto action_35
action_18 _ = happyFail

action_19 (9) = happyShift action_4
action_19 (14) = happyShift action_5
action_19 (16) = happyShift action_6
action_19 (25) = happyShift action_7
action_19 (7) = happyGoto action_34
action_19 _ = happyFail

action_20 (9) = happyShift action_4
action_20 (14) = happyShift action_5
action_20 (16) = happyShift action_6
action_20 (25) = happyShift action_7
action_20 (7) = happyGoto action_33
action_20 _ = happyFail

action_21 (9) = happyShift action_4
action_21 (14) = happyShift action_5
action_21 (16) = happyShift action_6
action_21 (25) = happyShift action_7
action_21 (7) = happyGoto action_32
action_21 _ = happyFail

action_22 (9) = happyShift action_4
action_22 (14) = happyShift action_5
action_22 (16) = happyShift action_6
action_22 (25) = happyShift action_7
action_22 (7) = happyGoto action_31
action_22 _ = happyFail

action_23 (9) = happyShift action_4
action_23 (14) = happyShift action_5
action_23 (16) = happyShift action_6
action_23 (25) = happyShift action_7
action_23 (7) = happyGoto action_30
action_23 _ = happyFail

action_24 (9) = happyShift action_4
action_24 (14) = happyShift action_5
action_24 (16) = happyShift action_6
action_24 (25) = happyShift action_7
action_24 (7) = happyGoto action_29
action_24 _ = happyFail

action_25 (9) = happyShift action_4
action_25 (14) = happyShift action_5
action_25 (16) = happyShift action_6
action_25 (25) = happyShift action_7
action_25 (7) = happyGoto action_28
action_25 _ = happyFail

action_26 (9) = happyShift action_4
action_26 (14) = happyShift action_5
action_26 (16) = happyShift action_6
action_26 (25) = happyShift action_7
action_26 (7) = happyGoto action_27
action_26 _ = happyFail

action_27 (12) = happyShift action_15
action_27 (15) = happyFail
action_27 (16) = happyShift action_18
action_27 (17) = happyShift action_19
action_27 (18) = happyShift action_20
action_27 (20) = happyShift action_22
action_27 (21) = happyShift action_23
action_27 (22) = happyShift action_24
action_27 (23) = happyShift action_25
action_27 (24) = happyFail
action_27 _ = happyReduce_12

action_28 (12) = happyShift action_15
action_28 (16) = happyShift action_18
action_28 (17) = happyShift action_19
action_28 (18) = happyShift action_20
action_28 (20) = happyFail
action_28 (21) = happyFail
action_28 (22) = happyFail
action_28 (23) = happyFail
action_28 _ = happyReduce_11

action_29 (12) = happyShift action_15
action_29 (16) = happyShift action_18
action_29 (17) = happyShift action_19
action_29 (18) = happyShift action_20
action_29 (20) = happyFail
action_29 (21) = happyFail
action_29 (22) = happyFail
action_29 (23) = happyFail
action_29 _ = happyReduce_7

action_30 (12) = happyShift action_15
action_30 (16) = happyShift action_18
action_30 (17) = happyShift action_19
action_30 (18) = happyShift action_20
action_30 (20) = happyFail
action_30 (21) = happyFail
action_30 (22) = happyFail
action_30 (23) = happyFail
action_30 _ = happyReduce_8

action_31 (12) = happyShift action_15
action_31 (16) = happyShift action_18
action_31 (17) = happyShift action_19
action_31 (18) = happyShift action_20
action_31 (20) = happyFail
action_31 (21) = happyFail
action_31 (22) = happyFail
action_31 (23) = happyFail
action_31 _ = happyReduce_6

action_32 (10) = happyShift action_14
action_32 (12) = happyShift action_15
action_32 (13) = happyShift action_16
action_32 (15) = happyShift action_17
action_32 (16) = happyShift action_18
action_32 (17) = happyShift action_19
action_32 (18) = happyShift action_20
action_32 (20) = happyShift action_22
action_32 (21) = happyShift action_23
action_32 (22) = happyShift action_24
action_32 (23) = happyShift action_25
action_32 (24) = happyShift action_26
action_32 _ = happyReduce_3

action_33 _ = happyReduce_18

action_34 _ = happyReduce_19

action_35 (17) = happyShift action_19
action_35 (18) = happyShift action_20
action_35 _ = happyReduce_16

action_36 (12) = happyShift action_15
action_36 (15) = happyFail
action_36 (16) = happyShift action_18
action_36 (17) = happyShift action_19
action_36 (18) = happyShift action_20
action_36 (20) = happyShift action_22
action_36 (21) = happyShift action_23
action_36 (22) = happyShift action_24
action_36 (23) = happyShift action_25
action_36 (24) = happyFail
action_36 _ = happyReduce_13

action_37 (12) = happyShift action_15
action_37 (15) = happyShift action_17
action_37 (16) = happyShift action_18
action_37 (17) = happyShift action_19
action_37 (18) = happyShift action_20
action_37 (20) = happyShift action_22
action_37 (21) = happyShift action_23
action_37 (22) = happyShift action_24
action_37 (23) = happyShift action_25
action_37 (24) = happyShift action_26
action_37 _ = happyReduce_9

action_38 (17) = happyShift action_19
action_38 (18) = happyShift action_20
action_38 _ = happyReduce_15

action_39 (12) = happyShift action_15
action_39 (13) = happyShift action_16
action_39 (15) = happyShift action_17
action_39 (16) = happyShift action_18
action_39 (17) = happyShift action_19
action_39 (18) = happyShift action_20
action_39 (20) = happyShift action_22
action_39 (21) = happyShift action_23
action_39 (22) = happyShift action_24
action_39 (23) = happyShift action_25
action_39 (24) = happyShift action_26
action_39 _ = happyReduce_10

action_40 _ = happyReduce_22

action_41 (8) = happyShift action_44
action_41 (11) = happyShift action_45
action_41 _ = happyFail

action_42 (10) = happyShift action_14
action_42 (12) = happyShift action_15
action_42 (13) = happyShift action_16
action_42 (15) = happyShift action_17
action_42 (16) = happyShift action_18
action_42 (17) = happyShift action_19
action_42 (18) = happyShift action_20
action_42 (20) = happyShift action_22
action_42 (21) = happyShift action_23
action_42 (22) = happyShift action_24
action_42 (23) = happyShift action_25
action_42 (24) = happyShift action_26
action_42 _ = happyReduce_4

action_43 _ = happyReduce_2

action_44 (9) = happyShift action_4
action_44 (14) = happyShift action_5
action_44 (16) = happyShift action_6
action_44 (25) = happyShift action_7
action_44 (7) = happyGoto action_46
action_44 _ = happyFail

action_45 _ = happyReduce_20

action_46 (10) = happyShift action_14
action_46 (12) = happyShift action_15
action_46 (13) = happyShift action_16
action_46 (15) = happyShift action_17
action_46 (16) = happyShift action_18
action_46 (17) = happyShift action_19
action_46 (18) = happyShift action_20
action_46 (20) = happyShift action_22
action_46 (21) = happyShift action_23
action_46 (22) = happyShift action_24
action_46 (23) = happyShift action_25
action_46 (24) = happyShift action_26
action_46 _ = happyReduce_5

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_3 : happy_var_1
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 ((happy_var_1, happy_var_3)
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_3 : happy_var_1
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Function "lt" [happy_var_1, happy_var_3]
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Function "leq" [happy_var_1, happy_var_3]
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  7 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Function "gt" [happy_var_1, happy_var_3]
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  7 happyReduction_9
happyReduction_9 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Function "and" [happy_var_1, happy_var_3]
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  7 happyReduction_10
happyReduction_10 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Function "or" [happy_var_1, happy_var_3]
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  7 happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Function "geq" [happy_var_1, happy_var_3]
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  7 happyReduction_12
happyReduction_12 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Function "eq" [happy_var_1, happy_var_3]
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  7 happyReduction_13
happyReduction_13 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Function "not" [Function "eq" [happy_var_1, happy_var_3]]
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  7 happyReduction_14
happyReduction_14 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Function "not" [happy_var_2]
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  7 happyReduction_15
happyReduction_15 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Function "plus" [happy_var_1, happy_var_3]
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  7 happyReduction_16
happyReduction_16 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Function "minus" [happy_var_1, happy_var_3]
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  7 happyReduction_17
happyReduction_17 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Function "multiply" [Variable "-1", happy_var_2]
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  7 happyReduction_18
happyReduction_18 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Function "multiply" [happy_var_1, happy_var_3]
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  7 happyReduction_19
happyReduction_19 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Function "divide" [happy_var_1, happy_var_3]
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 4 7 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Function happy_var_1 (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_1  7 happyReduction_21
happyReduction_21 (HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn7
		 (Variable happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  7 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TokenEOF -> action 26 26 tk (HappyState action) sts stk;
	TokenComma -> cont 8;
	TokenOB -> cont 9;
	TokenOr -> cont 10;
	TokenCB -> cont 11;
	TokenPlus -> cont 12;
	TokenAnd -> cont 13;
	TokenNot -> cont 14;
	TokenNotEqual -> cont 15;
	TokenMinus -> cont 16;
	TokenDivide -> cont 17;
	TokenMultiply -> cont 18;
	TokenBecomes -> cont 19;
	TokenSmaller -> cont 20;
	TokenGreater -> cont 21;
	TokenSmallerEq -> cont 22;
	TokenGreaterEq -> cont 23;
	TokenEqual -> cont 24;
	TokenString happy_dollar_dollar -> cont 25;
	_ -> happyError' tk
	})

happyError_ 26 tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => ParserMonad a -> (a -> ParserMonad b) -> ParserMonad b
happyThen = (thenParserMonad)
happyReturn :: () => a -> ParserMonad a
happyReturn = (returnParserMonad)
happyThen1 = happyThen
happyReturn1 :: () => a -> ParserMonad a
happyReturn1 = happyReturn
happyError' :: () => (Token) -> ParserMonad a
happyError' tk = parseError tk

parseExpression = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}





# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 5 "<command-line>" 2
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 45 "templates/GenericTemplate.hs" #-}








{-# LINE 66 "templates/GenericTemplate.hs" #-}

{-# LINE 76 "templates/GenericTemplate.hs" #-}

{-# LINE 85 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 154 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 255 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 321 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
