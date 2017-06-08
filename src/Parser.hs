{-# OPTIONS_GHC -w #-}
module Parser (
  parseProg,
) where

import Lexer
import Syntax
import Types

import Control.Monad.Except
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10

action_0 (12) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (12) = happyShift action_2
action_1 _ = happyFail

action_2 (16) = happyShift action_4
action_2 _ = happyFail

action_3 (36) = happyAccept
action_3 _ = happyFail

action_4 (30) = happyShift action_6
action_4 (5) = happyGoto action_5
action_4 _ = happyFail

action_5 _ = happyReduce_1

action_6 (12) = happyShift action_12
action_6 (13) = happyShift action_13
action_6 (14) = happyShift action_14
action_6 (15) = happyShift action_15
action_6 (17) = happyShift action_16
action_6 (18) = happyShift action_17
action_6 (19) = happyShift action_18
action_6 (20) = happyShift action_19
action_6 (30) = happyShift action_6
action_6 (5) = happyGoto action_7
action_6 (7) = happyGoto action_8
action_6 (8) = happyGoto action_9
action_6 (9) = happyGoto action_10
action_6 (10) = happyGoto action_11
action_6 _ = happyFail

action_7 _ = happyReduce_16

action_8 (12) = happyShift action_12
action_8 (13) = happyShift action_13
action_8 (14) = happyShift action_14
action_8 (15) = happyShift action_15
action_8 (17) = happyShift action_16
action_8 (18) = happyShift action_17
action_8 (19) = happyShift action_18
action_8 (20) = happyShift action_19
action_8 (30) = happyShift action_6
action_8 (31) = happyShift action_33
action_8 (5) = happyGoto action_7
action_8 (8) = happyGoto action_9
action_8 (9) = happyGoto action_32
action_8 (10) = happyGoto action_11
action_8 _ = happyFail

action_9 (18) = happyShift action_31
action_9 _ = happyFail

action_10 _ = happyReduce_5

action_11 (21) = happyShift action_29
action_11 (32) = happyShift action_30
action_11 _ = happyFail

action_12 _ = happyReduce_7

action_13 _ = happyReduce_8

action_14 _ = happyReduce_9

action_15 _ = happyReduce_27

action_16 _ = happyReduce_25

action_17 (23) = happyShift action_22
action_17 (24) = happyShift action_23
action_17 (25) = happyShift action_24
action_17 (26) = happyShift action_25
action_17 (27) = happyShift action_26
action_17 (34) = happyShift action_27
action_17 (35) = happyShift action_28
action_17 _ = happyReduce_26

action_18 (28) = happyShift action_21
action_18 _ = happyFail

action_19 (28) = happyShift action_20
action_19 _ = happyFail

action_20 (15) = happyShift action_15
action_20 (17) = happyShift action_16
action_20 (18) = happyShift action_37
action_20 (10) = happyGoto action_46
action_20 _ = happyFail

action_21 (15) = happyShift action_15
action_21 (17) = happyShift action_16
action_21 (18) = happyShift action_37
action_21 (10) = happyGoto action_45
action_21 _ = happyFail

action_22 (15) = happyShift action_15
action_22 (17) = happyShift action_16
action_22 (18) = happyShift action_37
action_22 (10) = happyGoto action_44
action_22 _ = happyFail

action_23 (15) = happyShift action_15
action_23 (17) = happyShift action_16
action_23 (18) = happyShift action_37
action_23 (10) = happyGoto action_43
action_23 _ = happyFail

action_24 (15) = happyShift action_15
action_24 (17) = happyShift action_16
action_24 (18) = happyShift action_37
action_24 (10) = happyGoto action_42
action_24 _ = happyFail

action_25 (15) = happyShift action_15
action_25 (17) = happyShift action_16
action_25 (18) = happyShift action_37
action_25 (10) = happyGoto action_41
action_25 _ = happyFail

action_26 (15) = happyShift action_15
action_26 (17) = happyShift action_16
action_26 (18) = happyShift action_37
action_26 (10) = happyGoto action_40
action_26 _ = happyFail

action_27 (15) = happyShift action_15
action_27 (17) = happyShift action_16
action_27 (18) = happyShift action_37
action_27 (10) = happyGoto action_39
action_27 _ = happyFail

action_28 (15) = happyShift action_15
action_28 (17) = happyShift action_16
action_28 (18) = happyShift action_37
action_28 (10) = happyGoto action_38
action_28 _ = happyFail

action_29 _ = happyReduce_10

action_30 (15) = happyShift action_15
action_30 (17) = happyShift action_16
action_30 (18) = happyShift action_37
action_30 (10) = happyGoto action_36
action_30 _ = happyFail

action_31 (21) = happyShift action_34
action_31 (26) = happyShift action_35
action_31 _ = happyFail

action_32 _ = happyReduce_6

action_33 _ = happyReduce_2

action_34 _ = happyReduce_12

action_35 (15) = happyShift action_15
action_35 (17) = happyShift action_16
action_35 (18) = happyShift action_37
action_35 (10) = happyGoto action_52
action_35 _ = happyFail

action_36 (32) = happyShift action_30
action_36 (33) = happyShift action_51
action_36 _ = happyFail

action_37 (23) = happyShift action_22
action_37 (24) = happyShift action_23
action_37 (25) = happyShift action_24
action_37 (26) = happyShift action_50
action_37 (27) = happyShift action_26
action_37 (34) = happyShift action_27
action_37 (35) = happyShift action_28
action_37 _ = happyReduce_26

action_38 (32) = happyShift action_30
action_38 _ = happyReduce_22

action_39 (32) = happyShift action_30
action_39 _ = happyReduce_21

action_40 (32) = happyShift action_30
action_40 _ = happyReduce_19

action_41 (21) = happyShift action_49
action_41 (32) = happyShift action_30
action_41 _ = happyFail

action_42 (32) = happyShift action_30
action_42 _ = happyReduce_20

action_43 (32) = happyShift action_30
action_43 _ = happyReduce_18

action_44 (32) = happyShift action_30
action_44 _ = happyReduce_17

action_45 (29) = happyShift action_48
action_45 (32) = happyShift action_30
action_45 _ = happyFail

action_46 (29) = happyShift action_47
action_46 (32) = happyShift action_30
action_46 _ = happyFail

action_47 (12) = happyShift action_12
action_47 (13) = happyShift action_13
action_47 (14) = happyShift action_14
action_47 (15) = happyShift action_15
action_47 (17) = happyShift action_16
action_47 (18) = happyShift action_17
action_47 (19) = happyShift action_18
action_47 (20) = happyShift action_19
action_47 (30) = happyShift action_6
action_47 (5) = happyGoto action_7
action_47 (8) = happyGoto action_9
action_47 (9) = happyGoto action_56
action_47 (10) = happyGoto action_11
action_47 _ = happyFail

action_48 (12) = happyShift action_12
action_48 (13) = happyShift action_13
action_48 (14) = happyShift action_14
action_48 (15) = happyShift action_15
action_48 (17) = happyShift action_16
action_48 (18) = happyShift action_17
action_48 (19) = happyShift action_18
action_48 (20) = happyShift action_19
action_48 (30) = happyShift action_6
action_48 (5) = happyGoto action_7
action_48 (8) = happyGoto action_9
action_48 (9) = happyGoto action_55
action_48 (10) = happyGoto action_11
action_48 _ = happyFail

action_49 _ = happyReduce_11

action_50 (15) = happyShift action_15
action_50 (17) = happyShift action_16
action_50 (18) = happyShift action_37
action_50 (10) = happyGoto action_54
action_50 _ = happyFail

action_51 _ = happyReduce_24

action_52 (21) = happyShift action_53
action_52 (32) = happyShift action_30
action_52 _ = happyFail

action_53 _ = happyReduce_13

action_54 (32) = happyShift action_30
action_54 _ = happyReduce_23

action_55 _ = happyReduce_14

action_56 _ = happyReduce_15

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_3)
	_
	_
	 =  HappyAbsSyn4
		 (Prog happy_var_3 emptyState
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Block happy_var_2 emptyState
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  6 happyReduction_3
happyReduction_3  =  HappyAbsSyn6
		 ([]
	)

happyReduce_4 = happyReduce 4 6 happyReduction_4
happyReduction_4 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_2)) `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 ((Arg happy_var_1 (Name happy_var_2)):happy_var_4
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_1  7 happyReduction_5
happyReduction_5 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn7
		 (Statements' happy_var_1 emptyState
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  7 happyReduction_6
happyReduction_6 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Statements happy_var_1 happy_var_2 emptyState
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  8 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn8
		 (Int
	)

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn8
		 (Bool
	)

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn8
		 (Char
	)

happyReduce_10 = happySpecReduce_2  9 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (SExpr happy_var_1 emptyState
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happyReduce 4 9 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (SAssign (Name happy_var_1) happy_var_3 emptyState
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_3  9 happyReduction_12
happyReduction_12 _
	(HappyTerminal (TokenSym happy_var_2))
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn9
		 (SDecl (Name happy_var_2) happy_var_1 emptyState
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 5 9 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_2)) `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (SDeclAssign (Name happy_var_2) happy_var_1 happy_var_4 emptyState
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 5 9 happyReduction_14
happyReduction_14 ((HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (SWhile happy_var_3 happy_var_5 emptyState
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 5 9 happyReduction_15
happyReduction_15 ((HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (SIf happy_var_3 happy_var_5 emptyState
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_1  9 happyReduction_16
happyReduction_16 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn9
		 (SBlock happy_var_1 emptyState
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  10 happyReduction_17
happyReduction_17 (HappyAbsSyn10  happy_var_3)
	_
	(HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn10
		 (BOp Plus (Name happy_var_1) happy_var_3 emptyState
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  10 happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_3)
	_
	(HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn10
		 (BOp Minus (Name happy_var_1) happy_var_3 emptyState
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  10 happyReduction_19
happyReduction_19 (HappyAbsSyn10  happy_var_3)
	_
	(HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn10
		 (BOp Times (Name happy_var_1) happy_var_3 emptyState
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  10 happyReduction_20
happyReduction_20 (HappyAbsSyn10  happy_var_3)
	_
	(HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn10
		 (BOp Div (Name happy_var_1) happy_var_3 emptyState
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  10 happyReduction_21
happyReduction_21 (HappyAbsSyn10  happy_var_3)
	_
	(HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn10
		 (BOp Lt (Name happy_var_1) happy_var_3 emptyState
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  10 happyReduction_22
happyReduction_22 (HappyAbsSyn10  happy_var_3)
	_
	(HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn10
		 (BOp Lte (Name happy_var_1) happy_var_3 emptyState
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  10 happyReduction_23
happyReduction_23 (HappyAbsSyn10  happy_var_3)
	_
	(HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn10
		 (BOp Assign (Name happy_var_1) happy_var_3 emptyState
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 4 10 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (BOp Access (Name happy_var_1) happy_var_3 emptyState
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_1  10 happyReduction_25
happyReduction_25 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn10
		 (Lit happy_var_1 emptyState
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  10 happyReduction_26
happyReduction_26 (HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn10
		 (Var (Name happy_var_1) emptyState
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  10 happyReduction_27
happyReduction_27 (HappyTerminal (TokenChar happy_var_1))
	 =  HappyAbsSyn10
		 (Ch happy_var_1 emptyState
	)
happyReduction_27 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 36 36 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenReturn -> cont 11;
	TokenIntDec -> cont 12;
	TokenBoolDec -> cont 13;
	TokenCharDec -> cont 14;
	TokenChar happy_dollar_dollar -> cont 15;
	TokenMain -> cont 16;
	TokenNum happy_dollar_dollar -> cont 17;
	TokenSym happy_dollar_dollar -> cont 18;
	TokenWhile -> cont 19;
	TokenIf -> cont 20;
	TokenSemi -> cont 21;
	TokenComma -> cont 22;
	TokenPlus -> cont 23;
	TokenMinus -> cont 24;
	TokenStar -> cont 25;
	TokenAssign -> cont 26;
	TokenDiv -> cont 27;
	TokenLparen -> cont 28;
	TokenRparen -> cont 29;
	TokenLbrace -> cont 30;
	TokenRbrace -> cont 31;
	TokenLbrack -> cont 32;
	TokenRbrack -> cont 33;
	TokenLt -> cont 34;
	TokenLte -> cont 35;
	_ -> happyError' (tk:tks)
	}

happyError_ 36 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Except String a -> (a -> Except String b) -> Except String b
happyThen = ((>>=))
happyReturn :: () => a -> Except String a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Except String a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> Except String a
happyError' = parseError

prog tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> Except String a
parseError [] = throwError "Unexpected end of input"
parseError a = throwError (show a)

parseProg :: String -> Either String Prog
parseProg input =
  let tokenStream = scanTokens input in
    runExcept (prog tokenStream)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4















































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc-7.10.3/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

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

{-# LINE 155 "templates/GenericTemplate.hs" #-}

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

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
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
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
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
