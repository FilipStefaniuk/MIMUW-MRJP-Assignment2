{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParLatte where
import AbsLatte
import LexLatte
import ErrM
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Ident)
	| HappyAbsSyn5 (Integer)
	| HappyAbsSyn6 (String)
	| HappyAbsSyn7 (Program)
	| HappyAbsSyn8 ([TopDef])
	| HappyAbsSyn9 (TopDef)
	| HappyAbsSyn10 ([ClassItemDef])
	| HappyAbsSyn11 (ClassItemDef)
	| HappyAbsSyn12 (FunDef)
	| HappyAbsSyn13 ([Arg])
	| HappyAbsSyn14 (Arg)
	| HappyAbsSyn15 (Block)
	| HappyAbsSyn16 ([Stmt])
	| HappyAbsSyn17 (Stmt)
	| HappyAbsSyn18 ([Item])
	| HappyAbsSyn19 (Item)
	| HappyAbsSyn20 (LVal)
	| HappyAbsSyn21 (Type)
	| HappyAbsSyn22 ([Expr])
	| HappyAbsSyn23 (Expr)
	| HappyAbsSyn31 (AddOp)
	| HappyAbsSyn32 (MulOp)
	| HappyAbsSyn33 (RelOp)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158 :: () => Int -> ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88 :: () => ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

action_0 (59) = happyShift action_9
action_0 (60) = happyShift action_10
action_0 (66) = happyShift action_11
action_0 (71) = happyShift action_12
action_0 (73) = happyShift action_13
action_0 (78) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 (9) = happyGoto action_6
action_0 (12) = happyGoto action_7
action_0 (21) = happyGoto action_8
action_0 _ = happyFail

action_1 (78) = happyShift action_2
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 _ = happyReduce_48

action_4 (81) = happyAccept
action_4 _ = happyFail

action_5 _ = happyReduce_4

action_6 (59) = happyShift action_9
action_6 (60) = happyShift action_10
action_6 (66) = happyShift action_11
action_6 (71) = happyShift action_12
action_6 (73) = happyShift action_13
action_6 (78) = happyShift action_2
action_6 (4) = happyGoto action_3
action_6 (8) = happyGoto action_17
action_6 (9) = happyGoto action_6
action_6 (12) = happyGoto action_7
action_6 (21) = happyGoto action_8
action_6 _ = happyReduce_5

action_7 _ = happyReduce_7

action_8 (57) = happyShift action_16
action_8 (78) = happyShift action_2
action_8 (4) = happyGoto action_15
action_8 _ = happyFail

action_9 _ = happyReduce_46

action_10 (78) = happyShift action_2
action_10 (4) = happyGoto action_14
action_10 _ = happyFail

action_11 _ = happyReduce_44

action_12 _ = happyReduce_45

action_13 _ = happyReduce_43

action_14 (62) = happyShift action_19
action_14 (75) = happyShift action_20
action_14 _ = happyFail

action_15 (38) = happyShift action_18
action_15 _ = happyFail

action_16 _ = happyReduce_47

action_17 _ = happyReduce_6

action_18 (59) = happyShift action_9
action_18 (66) = happyShift action_11
action_18 (71) = happyShift action_12
action_18 (73) = happyShift action_13
action_18 (78) = happyShift action_2
action_18 (4) = happyGoto action_3
action_18 (13) = happyGoto action_23
action_18 (14) = happyGoto action_24
action_18 (21) = happyGoto action_25
action_18 _ = happyReduce_15

action_19 (78) = happyShift action_2
action_19 (4) = happyGoto action_22
action_19 _ = happyFail

action_20 (10) = happyGoto action_21
action_20 _ = happyReduce_10

action_21 (59) = happyShift action_9
action_21 (66) = happyShift action_11
action_21 (71) = happyShift action_12
action_21 (73) = happyShift action_13
action_21 (77) = happyShift action_33
action_21 (78) = happyShift action_2
action_21 (4) = happyGoto action_3
action_21 (11) = happyGoto action_30
action_21 (12) = happyGoto action_31
action_21 (21) = happyGoto action_32
action_21 _ = happyFail

action_22 (75) = happyShift action_29
action_22 _ = happyFail

action_23 (39) = happyShift action_28
action_23 _ = happyFail

action_24 (43) = happyShift action_27
action_24 _ = happyReduce_16

action_25 (57) = happyShift action_16
action_25 (78) = happyShift action_2
action_25 (4) = happyGoto action_26
action_25 _ = happyFail

action_26 _ = happyReduce_18

action_27 (59) = happyShift action_9
action_27 (66) = happyShift action_11
action_27 (71) = happyShift action_12
action_27 (73) = happyShift action_13
action_27 (78) = happyShift action_2
action_27 (4) = happyGoto action_3
action_27 (13) = happyGoto action_38
action_27 (14) = happyGoto action_24
action_27 (21) = happyGoto action_25
action_27 _ = happyReduce_15

action_28 (75) = happyShift action_37
action_28 (15) = happyGoto action_36
action_28 _ = happyFail

action_29 (10) = happyGoto action_35
action_29 _ = happyReduce_10

action_30 _ = happyReduce_11

action_31 _ = happyReduce_13

action_32 (57) = happyShift action_16
action_32 (78) = happyShift action_2
action_32 (4) = happyGoto action_34
action_32 _ = happyFail

action_33 _ = happyReduce_8

action_34 (38) = happyShift action_18
action_34 (49) = happyShift action_41
action_34 _ = happyFail

action_35 (59) = happyShift action_9
action_35 (66) = happyShift action_11
action_35 (71) = happyShift action_12
action_35 (73) = happyShift action_13
action_35 (77) = happyShift action_40
action_35 (78) = happyShift action_2
action_35 (4) = happyGoto action_3
action_35 (11) = happyGoto action_30
action_35 (12) = happyGoto action_31
action_35 (21) = happyGoto action_32
action_35 _ = happyFail

action_36 _ = happyReduce_14

action_37 (16) = happyGoto action_39
action_37 _ = happyReduce_20

action_38 _ = happyReduce_17

action_39 (34) = happyShift action_57
action_39 (38) = happyShift action_58
action_39 (44) = happyShift action_59
action_39 (49) = happyShift action_60
action_39 (59) = happyShift action_9
action_39 (63) = happyShift action_61
action_39 (64) = happyShift action_62
action_39 (65) = happyShift action_63
action_39 (66) = happyShift action_11
action_39 (67) = happyShift action_64
action_39 (68) = happyShift action_65
action_39 (69) = happyShift action_66
action_39 (70) = happyShift action_67
action_39 (71) = happyShift action_12
action_39 (72) = happyShift action_68
action_39 (73) = happyShift action_13
action_39 (74) = happyShift action_69
action_39 (75) = happyShift action_37
action_39 (77) = happyShift action_70
action_39 (78) = happyShift action_2
action_39 (79) = happyShift action_71
action_39 (80) = happyShift action_72
action_39 (4) = happyGoto action_42
action_39 (5) = happyGoto action_43
action_39 (6) = happyGoto action_44
action_39 (15) = happyGoto action_45
action_39 (17) = happyGoto action_46
action_39 (20) = happyGoto action_47
action_39 (21) = happyGoto action_48
action_39 (23) = happyGoto action_49
action_39 (24) = happyGoto action_50
action_39 (25) = happyGoto action_51
action_39 (26) = happyGoto action_52
action_39 (27) = happyGoto action_53
action_39 (28) = happyGoto action_54
action_39 (29) = happyGoto action_55
action_39 (30) = happyGoto action_56
action_39 _ = happyFail

action_40 _ = happyReduce_9

action_41 _ = happyReduce_12

action_42 (38) = happyShift action_112
action_42 (57) = happyReduce_48
action_42 (78) = happyReduce_48
action_42 _ = happyReduce_39

action_43 _ = happyReduce_70

action_44 _ = happyReduce_71

action_45 _ = happyReduce_23

action_46 _ = happyReduce_21

action_47 (42) = happyShift action_109
action_47 (45) = happyShift action_110
action_47 (52) = happyShift action_111
action_47 _ = happyReduce_75

action_48 (57) = happyShift action_16
action_48 (78) = happyShift action_2
action_48 (4) = happyGoto action_106
action_48 (18) = happyGoto action_107
action_48 (19) = happyGoto action_108
action_48 _ = happyFail

action_49 (49) = happyShift action_105
action_49 _ = happyFail

action_50 (76) = happyShift action_104
action_50 _ = happyReduce_52

action_51 (35) = happyShift action_97
action_51 (37) = happyShift action_98
action_51 (50) = happyShift action_99
action_51 (51) = happyShift action_100
action_51 (53) = happyShift action_101
action_51 (54) = happyShift action_102
action_51 (55) = happyShift action_103
action_51 (33) = happyGoto action_96
action_51 _ = happyReduce_54

action_52 (41) = happyShift action_94
action_52 (44) = happyShift action_95
action_52 (31) = happyGoto action_93
action_52 _ = happyReduce_56

action_53 (36) = happyShift action_90
action_53 (40) = happyShift action_91
action_53 (47) = happyShift action_92
action_53 (32) = happyGoto action_89
action_53 _ = happyReduce_58

action_54 _ = happyReduce_60

action_55 _ = happyReduce_62

action_56 (46) = happyShift action_87
action_56 (56) = happyShift action_88
action_56 _ = happyReduce_66

action_57 (38) = happyShift action_83
action_57 (63) = happyShift action_61
action_57 (67) = happyShift action_64
action_57 (68) = happyShift action_65
action_57 (70) = happyShift action_67
action_57 (72) = happyShift action_68
action_57 (78) = happyShift action_2
action_57 (79) = happyShift action_71
action_57 (80) = happyShift action_72
action_57 (4) = happyGoto action_74
action_57 (5) = happyGoto action_43
action_57 (6) = happyGoto action_44
action_57 (20) = happyGoto action_75
action_57 (29) = happyGoto action_86
action_57 (30) = happyGoto action_56
action_57 _ = happyFail

action_58 (34) = happyShift action_57
action_58 (38) = happyShift action_58
action_58 (44) = happyShift action_59
action_58 (63) = happyShift action_61
action_58 (67) = happyShift action_64
action_58 (68) = happyShift action_65
action_58 (70) = happyShift action_67
action_58 (72) = happyShift action_68
action_58 (78) = happyShift action_2
action_58 (79) = happyShift action_71
action_58 (80) = happyShift action_72
action_58 (4) = happyGoto action_84
action_58 (5) = happyGoto action_43
action_58 (6) = happyGoto action_44
action_58 (20) = happyGoto action_75
action_58 (23) = happyGoto action_85
action_58 (24) = happyGoto action_50
action_58 (25) = happyGoto action_51
action_58 (26) = happyGoto action_52
action_58 (27) = happyGoto action_53
action_58 (28) = happyGoto action_54
action_58 (29) = happyGoto action_55
action_58 (30) = happyGoto action_56
action_58 _ = happyFail

action_59 (38) = happyShift action_83
action_59 (63) = happyShift action_61
action_59 (67) = happyShift action_64
action_59 (68) = happyShift action_65
action_59 (70) = happyShift action_67
action_59 (72) = happyShift action_68
action_59 (78) = happyShift action_2
action_59 (79) = happyShift action_71
action_59 (80) = happyShift action_72
action_59 (4) = happyGoto action_74
action_59 (5) = happyGoto action_43
action_59 (6) = happyGoto action_44
action_59 (20) = happyGoto action_75
action_59 (29) = happyGoto action_82
action_59 (30) = happyGoto action_56
action_59 _ = happyFail

action_60 _ = happyReduce_22

action_61 _ = happyReduce_73

action_62 (38) = happyShift action_81
action_62 _ = happyFail

action_63 (38) = happyShift action_80
action_63 _ = happyFail

action_64 (59) = happyShift action_9
action_64 (66) = happyShift action_11
action_64 (71) = happyShift action_12
action_64 (73) = happyShift action_13
action_64 (78) = happyShift action_2
action_64 (4) = happyGoto action_78
action_64 (21) = happyGoto action_79
action_64 _ = happyFail

action_65 _ = happyReduce_74

action_66 (34) = happyShift action_57
action_66 (38) = happyShift action_58
action_66 (44) = happyShift action_59
action_66 (49) = happyShift action_77
action_66 (63) = happyShift action_61
action_66 (67) = happyShift action_64
action_66 (68) = happyShift action_65
action_66 (70) = happyShift action_67
action_66 (72) = happyShift action_68
action_66 (78) = happyShift action_2
action_66 (79) = happyShift action_71
action_66 (80) = happyShift action_72
action_66 (4) = happyGoto action_74
action_66 (5) = happyGoto action_43
action_66 (6) = happyGoto action_44
action_66 (20) = happyGoto action_75
action_66 (23) = happyGoto action_76
action_66 (24) = happyGoto action_50
action_66 (25) = happyGoto action_51
action_66 (26) = happyGoto action_52
action_66 (27) = happyGoto action_53
action_66 (28) = happyGoto action_54
action_66 (29) = happyGoto action_55
action_66 (30) = happyGoto action_56
action_66 _ = happyFail

action_67 _ = happyReduce_42

action_68 _ = happyReduce_72

action_69 (38) = happyShift action_73
action_69 _ = happyFail

action_70 _ = happyReduce_19

action_71 _ = happyReduce_2

action_72 _ = happyReduce_3

action_73 (34) = happyShift action_57
action_73 (38) = happyShift action_58
action_73 (44) = happyShift action_59
action_73 (63) = happyShift action_61
action_73 (67) = happyShift action_64
action_73 (68) = happyShift action_65
action_73 (70) = happyShift action_67
action_73 (72) = happyShift action_68
action_73 (78) = happyShift action_2
action_73 (79) = happyShift action_71
action_73 (80) = happyShift action_72
action_73 (4) = happyGoto action_74
action_73 (5) = happyGoto action_43
action_73 (6) = happyGoto action_44
action_73 (20) = happyGoto action_75
action_73 (23) = happyGoto action_134
action_73 (24) = happyGoto action_50
action_73 (25) = happyGoto action_51
action_73 (26) = happyGoto action_52
action_73 (27) = happyGoto action_53
action_73 (28) = happyGoto action_54
action_73 (29) = happyGoto action_55
action_73 (30) = happyGoto action_56
action_73 _ = happyFail

action_74 (38) = happyShift action_112
action_74 _ = happyReduce_39

action_75 _ = happyReduce_75

action_76 (49) = happyShift action_133
action_76 _ = happyFail

action_77 _ = happyReduce_29

action_78 (56) = happyReduce_48
action_78 (57) = happyReduce_48
action_78 _ = happyReduce_67

action_79 (56) = happyShift action_132
action_79 (57) = happyShift action_16
action_79 _ = happyFail

action_80 (34) = happyShift action_57
action_80 (38) = happyShift action_58
action_80 (44) = happyShift action_59
action_80 (63) = happyShift action_61
action_80 (67) = happyShift action_64
action_80 (68) = happyShift action_65
action_80 (70) = happyShift action_67
action_80 (72) = happyShift action_68
action_80 (78) = happyShift action_2
action_80 (79) = happyShift action_71
action_80 (80) = happyShift action_72
action_80 (4) = happyGoto action_74
action_80 (5) = happyGoto action_43
action_80 (6) = happyGoto action_44
action_80 (20) = happyGoto action_75
action_80 (23) = happyGoto action_131
action_80 (24) = happyGoto action_50
action_80 (25) = happyGoto action_51
action_80 (26) = happyGoto action_52
action_80 (27) = happyGoto action_53
action_80 (28) = happyGoto action_54
action_80 (29) = happyGoto action_55
action_80 (30) = happyGoto action_56
action_80 _ = happyFail

action_81 (59) = happyShift action_9
action_81 (66) = happyShift action_11
action_81 (71) = happyShift action_12
action_81 (73) = happyShift action_13
action_81 (78) = happyShift action_2
action_81 (4) = happyGoto action_3
action_81 (21) = happyGoto action_130
action_81 _ = happyFail

action_82 _ = happyReduce_64

action_83 (34) = happyShift action_57
action_83 (38) = happyShift action_58
action_83 (44) = happyShift action_59
action_83 (63) = happyShift action_61
action_83 (67) = happyShift action_64
action_83 (68) = happyShift action_65
action_83 (70) = happyShift action_67
action_83 (72) = happyShift action_68
action_83 (78) = happyShift action_2
action_83 (79) = happyShift action_71
action_83 (80) = happyShift action_72
action_83 (4) = happyGoto action_74
action_83 (5) = happyGoto action_43
action_83 (6) = happyGoto action_44
action_83 (20) = happyGoto action_75
action_83 (23) = happyGoto action_85
action_83 (24) = happyGoto action_50
action_83 (25) = happyGoto action_51
action_83 (26) = happyGoto action_52
action_83 (27) = happyGoto action_53
action_83 (28) = happyGoto action_54
action_83 (29) = happyGoto action_55
action_83 (30) = happyGoto action_56
action_83 _ = happyFail

action_84 (38) = happyShift action_112
action_84 (39) = happyShift action_129
action_84 _ = happyReduce_39

action_85 (39) = happyShift action_128
action_85 _ = happyFail

action_86 _ = happyReduce_65

action_87 (78) = happyShift action_2
action_87 (4) = happyGoto action_127
action_87 _ = happyFail

action_88 (34) = happyShift action_57
action_88 (38) = happyShift action_58
action_88 (44) = happyShift action_59
action_88 (63) = happyShift action_61
action_88 (67) = happyShift action_64
action_88 (68) = happyShift action_65
action_88 (70) = happyShift action_67
action_88 (72) = happyShift action_68
action_88 (78) = happyShift action_2
action_88 (79) = happyShift action_71
action_88 (80) = happyShift action_72
action_88 (4) = happyGoto action_74
action_88 (5) = happyGoto action_43
action_88 (6) = happyGoto action_44
action_88 (20) = happyGoto action_75
action_88 (23) = happyGoto action_126
action_88 (24) = happyGoto action_50
action_88 (25) = happyGoto action_51
action_88 (26) = happyGoto action_52
action_88 (27) = happyGoto action_53
action_88 (28) = happyGoto action_54
action_88 (29) = happyGoto action_55
action_88 (30) = happyGoto action_56
action_88 _ = happyFail

action_89 (34) = happyShift action_57
action_89 (38) = happyShift action_58
action_89 (44) = happyShift action_59
action_89 (63) = happyShift action_61
action_89 (67) = happyShift action_64
action_89 (68) = happyShift action_65
action_89 (70) = happyShift action_67
action_89 (72) = happyShift action_68
action_89 (78) = happyShift action_2
action_89 (79) = happyShift action_71
action_89 (80) = happyShift action_72
action_89 (4) = happyGoto action_74
action_89 (5) = happyGoto action_43
action_89 (6) = happyGoto action_44
action_89 (20) = happyGoto action_75
action_89 (28) = happyGoto action_125
action_89 (29) = happyGoto action_55
action_89 (30) = happyGoto action_56
action_89 _ = happyFail

action_90 _ = happyReduce_82

action_91 _ = happyReduce_80

action_92 _ = happyReduce_81

action_93 (34) = happyShift action_57
action_93 (38) = happyShift action_58
action_93 (44) = happyShift action_59
action_93 (63) = happyShift action_61
action_93 (67) = happyShift action_64
action_93 (68) = happyShift action_65
action_93 (70) = happyShift action_67
action_93 (72) = happyShift action_68
action_93 (78) = happyShift action_2
action_93 (79) = happyShift action_71
action_93 (80) = happyShift action_72
action_93 (4) = happyGoto action_74
action_93 (5) = happyGoto action_43
action_93 (6) = happyGoto action_44
action_93 (20) = happyGoto action_75
action_93 (27) = happyGoto action_124
action_93 (28) = happyGoto action_54
action_93 (29) = happyGoto action_55
action_93 (30) = happyGoto action_56
action_93 _ = happyFail

action_94 _ = happyReduce_78

action_95 _ = happyReduce_79

action_96 (34) = happyShift action_57
action_96 (38) = happyShift action_58
action_96 (44) = happyShift action_59
action_96 (63) = happyShift action_61
action_96 (67) = happyShift action_64
action_96 (68) = happyShift action_65
action_96 (70) = happyShift action_67
action_96 (72) = happyShift action_68
action_96 (78) = happyShift action_2
action_96 (79) = happyShift action_71
action_96 (80) = happyShift action_72
action_96 (4) = happyGoto action_74
action_96 (5) = happyGoto action_43
action_96 (6) = happyGoto action_44
action_96 (20) = happyGoto action_75
action_96 (26) = happyGoto action_123
action_96 (27) = happyGoto action_53
action_96 (28) = happyGoto action_54
action_96 (29) = happyGoto action_55
action_96 (30) = happyGoto action_56
action_96 _ = happyFail

action_97 _ = happyReduce_88

action_98 (34) = happyShift action_57
action_98 (38) = happyShift action_58
action_98 (44) = happyShift action_59
action_98 (63) = happyShift action_61
action_98 (67) = happyShift action_64
action_98 (68) = happyShift action_65
action_98 (70) = happyShift action_67
action_98 (72) = happyShift action_68
action_98 (78) = happyShift action_2
action_98 (79) = happyShift action_71
action_98 (80) = happyShift action_72
action_98 (4) = happyGoto action_74
action_98 (5) = happyGoto action_43
action_98 (6) = happyGoto action_44
action_98 (20) = happyGoto action_75
action_98 (24) = happyGoto action_122
action_98 (25) = happyGoto action_51
action_98 (26) = happyGoto action_52
action_98 (27) = happyGoto action_53
action_98 (28) = happyGoto action_54
action_98 (29) = happyGoto action_55
action_98 (30) = happyGoto action_56
action_98 _ = happyFail

action_99 _ = happyReduce_83

action_100 _ = happyReduce_84

action_101 _ = happyReduce_87

action_102 _ = happyReduce_85

action_103 _ = happyReduce_86

action_104 (34) = happyShift action_57
action_104 (38) = happyShift action_58
action_104 (44) = happyShift action_59
action_104 (63) = happyShift action_61
action_104 (67) = happyShift action_64
action_104 (68) = happyShift action_65
action_104 (70) = happyShift action_67
action_104 (72) = happyShift action_68
action_104 (78) = happyShift action_2
action_104 (79) = happyShift action_71
action_104 (80) = happyShift action_72
action_104 (4) = happyGoto action_74
action_104 (5) = happyGoto action_43
action_104 (6) = happyGoto action_44
action_104 (20) = happyGoto action_75
action_104 (23) = happyGoto action_121
action_104 (24) = happyGoto action_50
action_104 (25) = happyGoto action_51
action_104 (26) = happyGoto action_52
action_104 (27) = happyGoto action_53
action_104 (28) = happyGoto action_54
action_104 (29) = happyGoto action_55
action_104 (30) = happyGoto action_56
action_104 _ = happyFail

action_105 _ = happyReduce_34

action_106 (52) = happyShift action_120
action_106 _ = happyReduce_37

action_107 (49) = happyShift action_119
action_107 _ = happyFail

action_108 (43) = happyShift action_118
action_108 _ = happyReduce_35

action_109 (49) = happyShift action_117
action_109 _ = happyFail

action_110 (49) = happyShift action_116
action_110 _ = happyFail

action_111 (34) = happyShift action_57
action_111 (38) = happyShift action_58
action_111 (44) = happyShift action_59
action_111 (63) = happyShift action_61
action_111 (67) = happyShift action_64
action_111 (68) = happyShift action_65
action_111 (70) = happyShift action_67
action_111 (72) = happyShift action_68
action_111 (78) = happyShift action_2
action_111 (79) = happyShift action_71
action_111 (80) = happyShift action_72
action_111 (4) = happyGoto action_74
action_111 (5) = happyGoto action_43
action_111 (6) = happyGoto action_44
action_111 (20) = happyGoto action_75
action_111 (23) = happyGoto action_115
action_111 (24) = happyGoto action_50
action_111 (25) = happyGoto action_51
action_111 (26) = happyGoto action_52
action_111 (27) = happyGoto action_53
action_111 (28) = happyGoto action_54
action_111 (29) = happyGoto action_55
action_111 (30) = happyGoto action_56
action_111 _ = happyFail

action_112 (34) = happyShift action_57
action_112 (38) = happyShift action_58
action_112 (44) = happyShift action_59
action_112 (63) = happyShift action_61
action_112 (67) = happyShift action_64
action_112 (68) = happyShift action_65
action_112 (70) = happyShift action_67
action_112 (72) = happyShift action_68
action_112 (78) = happyShift action_2
action_112 (79) = happyShift action_71
action_112 (80) = happyShift action_72
action_112 (4) = happyGoto action_74
action_112 (5) = happyGoto action_43
action_112 (6) = happyGoto action_44
action_112 (20) = happyGoto action_75
action_112 (22) = happyGoto action_113
action_112 (23) = happyGoto action_114
action_112 (24) = happyGoto action_50
action_112 (25) = happyGoto action_51
action_112 (26) = happyGoto action_52
action_112 (27) = happyGoto action_53
action_112 (28) = happyGoto action_54
action_112 (29) = happyGoto action_55
action_112 (30) = happyGoto action_56
action_112 _ = happyReduce_49

action_113 (39) = happyShift action_146
action_113 _ = happyFail

action_114 (43) = happyShift action_145
action_114 _ = happyReduce_50

action_115 (49) = happyShift action_144
action_115 _ = happyFail

action_116 _ = happyReduce_27

action_117 _ = happyReduce_26

action_118 (78) = happyShift action_2
action_118 (4) = happyGoto action_106
action_118 (18) = happyGoto action_143
action_118 (19) = happyGoto action_108
action_118 _ = happyFail

action_119 _ = happyReduce_24

action_120 (34) = happyShift action_57
action_120 (38) = happyShift action_58
action_120 (44) = happyShift action_59
action_120 (63) = happyShift action_61
action_120 (67) = happyShift action_64
action_120 (68) = happyShift action_65
action_120 (70) = happyShift action_67
action_120 (72) = happyShift action_68
action_120 (78) = happyShift action_2
action_120 (79) = happyShift action_71
action_120 (80) = happyShift action_72
action_120 (4) = happyGoto action_74
action_120 (5) = happyGoto action_43
action_120 (6) = happyGoto action_44
action_120 (20) = happyGoto action_75
action_120 (23) = happyGoto action_142
action_120 (24) = happyGoto action_50
action_120 (25) = happyGoto action_51
action_120 (26) = happyGoto action_52
action_120 (27) = happyGoto action_53
action_120 (28) = happyGoto action_54
action_120 (29) = happyGoto action_55
action_120 (30) = happyGoto action_56
action_120 _ = happyFail

action_121 _ = happyReduce_53

action_122 _ = happyReduce_55

action_123 (41) = happyShift action_94
action_123 (44) = happyShift action_95
action_123 (31) = happyGoto action_93
action_123 _ = happyReduce_57

action_124 (36) = happyShift action_90
action_124 (40) = happyShift action_91
action_124 (47) = happyShift action_92
action_124 (32) = happyGoto action_89
action_124 _ = happyReduce_59

action_125 _ = happyReduce_61

action_126 (58) = happyShift action_141
action_126 _ = happyFail

action_127 (38) = happyShift action_140
action_127 _ = happyReduce_41

action_128 _ = happyReduce_69

action_129 (38) = happyShift action_83
action_129 (63) = happyShift action_61
action_129 (67) = happyShift action_64
action_129 (68) = happyShift action_65
action_129 (70) = happyShift action_67
action_129 (72) = happyShift action_68
action_129 (78) = happyShift action_2
action_129 (79) = happyShift action_71
action_129 (80) = happyShift action_72
action_129 (4) = happyGoto action_74
action_129 (5) = happyGoto action_43
action_129 (6) = happyGoto action_44
action_129 (20) = happyGoto action_75
action_129 (29) = happyGoto action_139
action_129 (30) = happyGoto action_56
action_129 _ = happyFail

action_130 (57) = happyShift action_16
action_130 (78) = happyShift action_2
action_130 (4) = happyGoto action_138
action_130 _ = happyFail

action_131 (39) = happyShift action_137
action_131 _ = happyFail

action_132 (34) = happyShift action_57
action_132 (38) = happyShift action_58
action_132 (44) = happyShift action_59
action_132 (63) = happyShift action_61
action_132 (67) = happyShift action_64
action_132 (68) = happyShift action_65
action_132 (70) = happyShift action_67
action_132 (72) = happyShift action_68
action_132 (78) = happyShift action_2
action_132 (79) = happyShift action_71
action_132 (80) = happyShift action_72
action_132 (4) = happyGoto action_74
action_132 (5) = happyGoto action_43
action_132 (6) = happyGoto action_44
action_132 (20) = happyGoto action_75
action_132 (23) = happyGoto action_136
action_132 (24) = happyGoto action_50
action_132 (25) = happyGoto action_51
action_132 (26) = happyGoto action_52
action_132 (27) = happyGoto action_53
action_132 (28) = happyGoto action_54
action_132 (29) = happyGoto action_55
action_132 (30) = happyGoto action_56
action_132 _ = happyFail

action_133 _ = happyReduce_28

action_134 (39) = happyShift action_135
action_134 _ = happyFail

action_135 (34) = happyShift action_57
action_135 (38) = happyShift action_58
action_135 (44) = happyShift action_59
action_135 (49) = happyShift action_60
action_135 (59) = happyShift action_9
action_135 (63) = happyShift action_61
action_135 (64) = happyShift action_62
action_135 (65) = happyShift action_63
action_135 (66) = happyShift action_11
action_135 (67) = happyShift action_64
action_135 (68) = happyShift action_65
action_135 (69) = happyShift action_66
action_135 (70) = happyShift action_67
action_135 (71) = happyShift action_12
action_135 (72) = happyShift action_68
action_135 (73) = happyShift action_13
action_135 (74) = happyShift action_69
action_135 (75) = happyShift action_37
action_135 (78) = happyShift action_2
action_135 (79) = happyShift action_71
action_135 (80) = happyShift action_72
action_135 (4) = happyGoto action_42
action_135 (5) = happyGoto action_43
action_135 (6) = happyGoto action_44
action_135 (15) = happyGoto action_45
action_135 (17) = happyGoto action_152
action_135 (20) = happyGoto action_47
action_135 (21) = happyGoto action_48
action_135 (23) = happyGoto action_49
action_135 (24) = happyGoto action_50
action_135 (25) = happyGoto action_51
action_135 (26) = happyGoto action_52
action_135 (27) = happyGoto action_53
action_135 (28) = happyGoto action_54
action_135 (29) = happyGoto action_55
action_135 (30) = happyGoto action_56
action_135 _ = happyFail

action_136 (58) = happyShift action_151
action_136 _ = happyFail

action_137 (34) = happyShift action_57
action_137 (38) = happyShift action_58
action_137 (44) = happyShift action_59
action_137 (49) = happyShift action_60
action_137 (59) = happyShift action_9
action_137 (63) = happyShift action_61
action_137 (64) = happyShift action_62
action_137 (65) = happyShift action_63
action_137 (66) = happyShift action_11
action_137 (67) = happyShift action_64
action_137 (68) = happyShift action_65
action_137 (69) = happyShift action_66
action_137 (70) = happyShift action_67
action_137 (71) = happyShift action_12
action_137 (72) = happyShift action_68
action_137 (73) = happyShift action_13
action_137 (74) = happyShift action_69
action_137 (75) = happyShift action_37
action_137 (78) = happyShift action_2
action_137 (79) = happyShift action_71
action_137 (80) = happyShift action_72
action_137 (4) = happyGoto action_42
action_137 (5) = happyGoto action_43
action_137 (6) = happyGoto action_44
action_137 (15) = happyGoto action_45
action_137 (17) = happyGoto action_150
action_137 (20) = happyGoto action_47
action_137 (21) = happyGoto action_48
action_137 (23) = happyGoto action_49
action_137 (24) = happyGoto action_50
action_137 (25) = happyGoto action_51
action_137 (26) = happyGoto action_52
action_137 (27) = happyGoto action_53
action_137 (28) = happyGoto action_54
action_137 (29) = happyGoto action_55
action_137 (30) = happyGoto action_56
action_137 _ = happyFail

action_138 (48) = happyShift action_149
action_138 _ = happyFail

action_139 _ = happyReduce_63

action_140 (34) = happyShift action_57
action_140 (38) = happyShift action_58
action_140 (44) = happyShift action_59
action_140 (63) = happyShift action_61
action_140 (67) = happyShift action_64
action_140 (68) = happyShift action_65
action_140 (70) = happyShift action_67
action_140 (72) = happyShift action_68
action_140 (78) = happyShift action_2
action_140 (79) = happyShift action_71
action_140 (80) = happyShift action_72
action_140 (4) = happyGoto action_74
action_140 (5) = happyGoto action_43
action_140 (6) = happyGoto action_44
action_140 (20) = happyGoto action_75
action_140 (22) = happyGoto action_148
action_140 (23) = happyGoto action_114
action_140 (24) = happyGoto action_50
action_140 (25) = happyGoto action_51
action_140 (26) = happyGoto action_52
action_140 (27) = happyGoto action_53
action_140 (28) = happyGoto action_54
action_140 (29) = happyGoto action_55
action_140 (30) = happyGoto action_56
action_140 _ = happyReduce_49

action_141 _ = happyReduce_40

action_142 _ = happyReduce_38

action_143 _ = happyReduce_36

action_144 _ = happyReduce_25

action_145 (34) = happyShift action_57
action_145 (38) = happyShift action_58
action_145 (44) = happyShift action_59
action_145 (63) = happyShift action_61
action_145 (67) = happyShift action_64
action_145 (68) = happyShift action_65
action_145 (70) = happyShift action_67
action_145 (72) = happyShift action_68
action_145 (78) = happyShift action_2
action_145 (79) = happyShift action_71
action_145 (80) = happyShift action_72
action_145 (4) = happyGoto action_74
action_145 (5) = happyGoto action_43
action_145 (6) = happyGoto action_44
action_145 (20) = happyGoto action_75
action_145 (22) = happyGoto action_147
action_145 (23) = happyGoto action_114
action_145 (24) = happyGoto action_50
action_145 (25) = happyGoto action_51
action_145 (26) = happyGoto action_52
action_145 (27) = happyGoto action_53
action_145 (28) = happyGoto action_54
action_145 (29) = happyGoto action_55
action_145 (30) = happyGoto action_56
action_145 _ = happyReduce_49

action_146 _ = happyReduce_76

action_147 _ = happyReduce_51

action_148 (39) = happyShift action_155
action_148 _ = happyFail

action_149 (34) = happyShift action_57
action_149 (38) = happyShift action_58
action_149 (44) = happyShift action_59
action_149 (63) = happyShift action_61
action_149 (67) = happyShift action_64
action_149 (68) = happyShift action_65
action_149 (70) = happyShift action_67
action_149 (72) = happyShift action_68
action_149 (78) = happyShift action_2
action_149 (79) = happyShift action_71
action_149 (80) = happyShift action_72
action_149 (4) = happyGoto action_74
action_149 (5) = happyGoto action_43
action_149 (6) = happyGoto action_44
action_149 (20) = happyGoto action_75
action_149 (23) = happyGoto action_154
action_149 (24) = happyGoto action_50
action_149 (25) = happyGoto action_51
action_149 (26) = happyGoto action_52
action_149 (27) = happyGoto action_53
action_149 (28) = happyGoto action_54
action_149 (29) = happyGoto action_55
action_149 (30) = happyGoto action_56
action_149 _ = happyFail

action_150 (61) = happyShift action_153
action_150 _ = happyReduce_30

action_151 _ = happyReduce_68

action_152 _ = happyReduce_32

action_153 (34) = happyShift action_57
action_153 (38) = happyShift action_58
action_153 (44) = happyShift action_59
action_153 (49) = happyShift action_60
action_153 (59) = happyShift action_9
action_153 (63) = happyShift action_61
action_153 (64) = happyShift action_62
action_153 (65) = happyShift action_63
action_153 (66) = happyShift action_11
action_153 (67) = happyShift action_64
action_153 (68) = happyShift action_65
action_153 (69) = happyShift action_66
action_153 (70) = happyShift action_67
action_153 (71) = happyShift action_12
action_153 (72) = happyShift action_68
action_153 (73) = happyShift action_13
action_153 (74) = happyShift action_69
action_153 (75) = happyShift action_37
action_153 (78) = happyShift action_2
action_153 (79) = happyShift action_71
action_153 (80) = happyShift action_72
action_153 (4) = happyGoto action_42
action_153 (5) = happyGoto action_43
action_153 (6) = happyGoto action_44
action_153 (15) = happyGoto action_45
action_153 (17) = happyGoto action_157
action_153 (20) = happyGoto action_47
action_153 (21) = happyGoto action_48
action_153 (23) = happyGoto action_49
action_153 (24) = happyGoto action_50
action_153 (25) = happyGoto action_51
action_153 (26) = happyGoto action_52
action_153 (27) = happyGoto action_53
action_153 (28) = happyGoto action_54
action_153 (29) = happyGoto action_55
action_153 (30) = happyGoto action_56
action_153 _ = happyFail

action_154 (39) = happyShift action_156
action_154 _ = happyFail

action_155 _ = happyReduce_77

action_156 (34) = happyShift action_57
action_156 (38) = happyShift action_58
action_156 (44) = happyShift action_59
action_156 (49) = happyShift action_60
action_156 (59) = happyShift action_9
action_156 (63) = happyShift action_61
action_156 (64) = happyShift action_62
action_156 (65) = happyShift action_63
action_156 (66) = happyShift action_11
action_156 (67) = happyShift action_64
action_156 (68) = happyShift action_65
action_156 (69) = happyShift action_66
action_156 (70) = happyShift action_67
action_156 (71) = happyShift action_12
action_156 (72) = happyShift action_68
action_156 (73) = happyShift action_13
action_156 (74) = happyShift action_69
action_156 (75) = happyShift action_37
action_156 (78) = happyShift action_2
action_156 (79) = happyShift action_71
action_156 (80) = happyShift action_72
action_156 (4) = happyGoto action_42
action_156 (5) = happyGoto action_43
action_156 (6) = happyGoto action_44
action_156 (15) = happyGoto action_45
action_156 (17) = happyGoto action_158
action_156 (20) = happyGoto action_47
action_156 (21) = happyGoto action_48
action_156 (23) = happyGoto action_49
action_156 (24) = happyGoto action_50
action_156 (25) = happyGoto action_51
action_156 (26) = happyGoto action_52
action_156 (27) = happyGoto action_53
action_156 (28) = happyGoto action_54
action_156 (29) = happyGoto action_55
action_156 (30) = happyGoto action_56
action_156 _ = happyFail

action_157 _ = happyReduce_31

action_158 _ = happyReduce_33

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (PT _ (TV happy_var_1)))
	 =  HappyAbsSyn4
		 (Ident happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn5
		 ((read ( happy_var_1)) :: Integer
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (AbsLatte.Prog happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  8 happyReduction_5
happyReduction_5 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 ((:[]) happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  8 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  9 happyReduction_7
happyReduction_7 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn9
		 (AbsLatte.TopFunDef happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happyReduce 5 9 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (AbsLatte.ClassDef happy_var_2 (reverse happy_var_4)
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 7 9 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (AbsLatte.ClassExtDef happy_var_2 happy_var_4 (reverse happy_var_6)
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_0  10 happyReduction_10
happyReduction_10  =  HappyAbsSyn10
		 ([]
	)

happyReduce_11 = happySpecReduce_2  10 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  11 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn11
		 (AbsLatte.AttrDef happy_var_1 happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (AbsLatte.MethodDef happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happyReduce 6 12 happyReduction_14
happyReduction_14 ((HappyAbsSyn15  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (AbsLatte.FunDef happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_0  13 happyReduction_15
happyReduction_15  =  HappyAbsSyn13
		 ([]
	)

happyReduce_16 = happySpecReduce_1  13 happyReduction_16
happyReduction_16 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 ((:[]) happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  13 happyReduction_17
happyReduction_17 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  14 happyReduction_18
happyReduction_18 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn14
		 (AbsLatte.Ar happy_var_1 happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  15 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (AbsLatte.Blk (reverse happy_var_2)
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_0  16 happyReduction_20
happyReduction_20  =  HappyAbsSyn16
		 ([]
	)

happyReduce_21 = happySpecReduce_2  16 happyReduction_21
happyReduction_21 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  17 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn17
		 (AbsLatte.Empty
	)

happyReduce_23 = happySpecReduce_1  17 happyReduction_23
happyReduction_23 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn17
		 (AbsLatte.BStmt happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  17 happyReduction_24
happyReduction_24 _
	(HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn17
		 (AbsLatte.Decl happy_var_1 happy_var_2
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happyReduce 4 17 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (AbsLatte.Ass happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_3  17 happyReduction_26
happyReduction_26 _
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn17
		 (AbsLatte.Incr happy_var_1
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  17 happyReduction_27
happyReduction_27 _
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn17
		 (AbsLatte.Decr happy_var_1
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  17 happyReduction_28
happyReduction_28 _
	(HappyAbsSyn23  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (AbsLatte.Ret happy_var_2
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  17 happyReduction_29
happyReduction_29 _
	_
	 =  HappyAbsSyn17
		 (AbsLatte.VRet
	)

happyReduce_30 = happyReduce 5 17 happyReduction_30
happyReduction_30 ((HappyAbsSyn17  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (AbsLatte.Cond happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 7 17 happyReduction_31
happyReduction_31 ((HappyAbsSyn17  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (AbsLatte.CondElse happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 5 17 happyReduction_32
happyReduction_32 ((HappyAbsSyn17  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (AbsLatte.While happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_33 = happyReduce 8 17 happyReduction_33
happyReduction_33 ((HappyAbsSyn17  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyAbsSyn21  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (AbsLatte.For happy_var_3 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_34 = happySpecReduce_2  17 happyReduction_34
happyReduction_34 _
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn17
		 (AbsLatte.SExp happy_var_1
	)
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  18 happyReduction_35
happyReduction_35 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 ((:[]) happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  18 happyReduction_36
happyReduction_36 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  19 happyReduction_37
happyReduction_37 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn19
		 (AbsLatte.NoInit happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  19 happyReduction_38
happyReduction_38 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn19
		 (AbsLatte.Init happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  20 happyReduction_39
happyReduction_39 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn20
		 (AbsLatte.LVar happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happyReduce 4 20 happyReduction_40
happyReduction_40 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (AbsLatte.LArr happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_41 = happySpecReduce_3  20 happyReduction_41
happyReduction_41 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn20
		 (AbsLatte.LAttr happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  20 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn20
		 (AbsLatte.LSelf
	)

happyReduce_43 = happySpecReduce_1  21 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn21
		 (AbsLatte.Void
	)

happyReduce_44 = happySpecReduce_1  21 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn21
		 (AbsLatte.Int
	)

happyReduce_45 = happySpecReduce_1  21 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn21
		 (AbsLatte.Str
	)

happyReduce_46 = happySpecReduce_1  21 happyReduction_46
happyReduction_46 _
	 =  HappyAbsSyn21
		 (AbsLatte.Bool
	)

happyReduce_47 = happySpecReduce_2  21 happyReduction_47
happyReduction_47 _
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (AbsLatte.Arr happy_var_1
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  21 happyReduction_48
happyReduction_48 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn21
		 (AbsLatte.Obj happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_0  22 happyReduction_49
happyReduction_49  =  HappyAbsSyn22
		 ([]
	)

happyReduce_50 = happySpecReduce_1  22 happyReduction_50
happyReduction_50 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 ((:[]) happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  22 happyReduction_51
happyReduction_51 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  23 happyReduction_52
happyReduction_52 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  23 happyReduction_53
happyReduction_53 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (AbsLatte.EOr happy_var_1 happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  24 happyReduction_54
happyReduction_54 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  24 happyReduction_55
happyReduction_55 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (AbsLatte.EAnd happy_var_1 happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  25 happyReduction_56
happyReduction_56 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  25 happyReduction_57
happyReduction_57 (HappyAbsSyn23  happy_var_3)
	(HappyAbsSyn33  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (AbsLatte.ERel happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  26 happyReduction_58
happyReduction_58 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_3  26 happyReduction_59
happyReduction_59 (HappyAbsSyn23  happy_var_3)
	(HappyAbsSyn31  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (AbsLatte.EAdd happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  27 happyReduction_60
happyReduction_60 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  27 happyReduction_61
happyReduction_61 (HappyAbsSyn23  happy_var_3)
	(HappyAbsSyn32  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (AbsLatte.EMul happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  28 happyReduction_62
happyReduction_62 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happyReduce 4 28 happyReduction_63
happyReduction_63 ((HappyAbsSyn23  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (AbsLatte.ECast happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_64 = happySpecReduce_2  28 happyReduction_64
happyReduction_64 (HappyAbsSyn23  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (AbsLatte.Neg happy_var_2
	)
happyReduction_64 _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_2  28 happyReduction_65
happyReduction_65 (HappyAbsSyn23  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (AbsLatte.Not happy_var_2
	)
happyReduction_65 _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  29 happyReduction_66
happyReduction_66 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_2  29 happyReduction_67
happyReduction_67 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (AbsLatte.ENewObj happy_var_2
	)
happyReduction_67 _ _  = notHappyAtAll 

happyReduce_68 = happyReduce 5 29 happyReduction_68
happyReduction_68 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (AbsLatte.ENewArr happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_69 = happySpecReduce_3  30 happyReduction_69
happyReduction_69 _
	(HappyAbsSyn23  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (happy_var_2
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  30 happyReduction_70
happyReduction_70 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn23
		 (AbsLatte.ELitInt happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  30 happyReduction_71
happyReduction_71 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn23
		 (AbsLatte.EString happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  30 happyReduction_72
happyReduction_72 _
	 =  HappyAbsSyn23
		 (AbsLatte.ELitTrue
	)

happyReduce_73 = happySpecReduce_1  30 happyReduction_73
happyReduction_73 _
	 =  HappyAbsSyn23
		 (AbsLatte.ELitFalse
	)

happyReduce_74 = happySpecReduce_1  30 happyReduction_74
happyReduction_74 _
	 =  HappyAbsSyn23
		 (AbsLatte.ENull
	)

happyReduce_75 = happySpecReduce_1  30 happyReduction_75
happyReduction_75 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn23
		 (AbsLatte.EVar happy_var_1
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happyReduce 4 30 happyReduction_76
happyReduction_76 (_ `HappyStk`
	(HappyAbsSyn22  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (AbsLatte.ECall happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_77 = happyReduce 6 30 happyReduction_77
happyReduction_77 (_ `HappyStk`
	(HappyAbsSyn22  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (AbsLatte.EMetCall happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_78 = happySpecReduce_1  31 happyReduction_78
happyReduction_78 _
	 =  HappyAbsSyn31
		 (AbsLatte.Plus
	)

happyReduce_79 = happySpecReduce_1  31 happyReduction_79
happyReduction_79 _
	 =  HappyAbsSyn31
		 (AbsLatte.Minus
	)

happyReduce_80 = happySpecReduce_1  32 happyReduction_80
happyReduction_80 _
	 =  HappyAbsSyn32
		 (AbsLatte.Times
	)

happyReduce_81 = happySpecReduce_1  32 happyReduction_81
happyReduction_81 _
	 =  HappyAbsSyn32
		 (AbsLatte.Div
	)

happyReduce_82 = happySpecReduce_1  32 happyReduction_82
happyReduction_82 _
	 =  HappyAbsSyn32
		 (AbsLatte.Mod
	)

happyReduce_83 = happySpecReduce_1  33 happyReduction_83
happyReduction_83 _
	 =  HappyAbsSyn33
		 (AbsLatte.LTH
	)

happyReduce_84 = happySpecReduce_1  33 happyReduction_84
happyReduction_84 _
	 =  HappyAbsSyn33
		 (AbsLatte.LE
	)

happyReduce_85 = happySpecReduce_1  33 happyReduction_85
happyReduction_85 _
	 =  HappyAbsSyn33
		 (AbsLatte.GTH
	)

happyReduce_86 = happySpecReduce_1  33 happyReduction_86
happyReduction_86 _
	 =  HappyAbsSyn33
		 (AbsLatte.GE
	)

happyReduce_87 = happySpecReduce_1  33 happyReduction_87
happyReduction_87 _
	 =  HappyAbsSyn33
		 (AbsLatte.EQU
	)

happyReduce_88 = happySpecReduce_1  33 happyReduction_88
happyReduction_88 _
	 =  HappyAbsSyn33
		 (AbsLatte.NE
	)

happyNewToken action sts stk [] =
	action 81 81 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 34;
	PT _ (TS _ 2) -> cont 35;
	PT _ (TS _ 3) -> cont 36;
	PT _ (TS _ 4) -> cont 37;
	PT _ (TS _ 5) -> cont 38;
	PT _ (TS _ 6) -> cont 39;
	PT _ (TS _ 7) -> cont 40;
	PT _ (TS _ 8) -> cont 41;
	PT _ (TS _ 9) -> cont 42;
	PT _ (TS _ 10) -> cont 43;
	PT _ (TS _ 11) -> cont 44;
	PT _ (TS _ 12) -> cont 45;
	PT _ (TS _ 13) -> cont 46;
	PT _ (TS _ 14) -> cont 47;
	PT _ (TS _ 15) -> cont 48;
	PT _ (TS _ 16) -> cont 49;
	PT _ (TS _ 17) -> cont 50;
	PT _ (TS _ 18) -> cont 51;
	PT _ (TS _ 19) -> cont 52;
	PT _ (TS _ 20) -> cont 53;
	PT _ (TS _ 21) -> cont 54;
	PT _ (TS _ 22) -> cont 55;
	PT _ (TS _ 23) -> cont 56;
	PT _ (TS _ 24) -> cont 57;
	PT _ (TS _ 25) -> cont 58;
	PT _ (TS _ 26) -> cont 59;
	PT _ (TS _ 27) -> cont 60;
	PT _ (TS _ 28) -> cont 61;
	PT _ (TS _ 29) -> cont 62;
	PT _ (TS _ 30) -> cont 63;
	PT _ (TS _ 31) -> cont 64;
	PT _ (TS _ 32) -> cont 65;
	PT _ (TS _ 33) -> cont 66;
	PT _ (TS _ 34) -> cont 67;
	PT _ (TS _ 35) -> cont 68;
	PT _ (TS _ 36) -> cont 69;
	PT _ (TS _ 37) -> cont 70;
	PT _ (TS _ 38) -> cont 71;
	PT _ (TS _ 39) -> cont 72;
	PT _ (TS _ 40) -> cont 73;
	PT _ (TS _ 41) -> cont 74;
	PT _ (TS _ 42) -> cont 75;
	PT _ (TS _ 43) -> cont 76;
	PT _ (TS _ 44) -> cont 77;
	PT _ (TV happy_dollar_dollar) -> cont 78;
	PT _ (TI happy_dollar_dollar) -> cont 79;
	PT _ (TL happy_dollar_dollar) -> cont 80;
	_ -> happyError' (tk:tks)
	}

happyError_ 81 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pProgram tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 10 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc9827_0/ghc_2.h" #-}


































































































































































{-# LINE 10 "<command-line>" #-}
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
