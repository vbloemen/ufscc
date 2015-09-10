{-# OPTIONS_GHC -w #-}
module Parser where

import ParserAux
import Data.Char
import Auxiliary
import DataSpec
import Expressions
import Processes

-- parser produced by Happy Version 1.19.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35 t36 t37 t38 t39
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30
	| HappyAbsSyn31 t31
	| HappyAbsSyn32 t32
	| HappyAbsSyn33 t33
	| HappyAbsSyn34 t34
	| HappyAbsSyn35 t35
	| HappyAbsSyn36 t36
	| HappyAbsSyn37 t37
	| HappyAbsSyn38 t38
	| HappyAbsSyn39 t39

action_0 (68) = happyShift action_4
action_0 (75) = happyShift action_5
action_0 (76) = happyShift action_6
action_0 (77) = happyShift action_7
action_0 (78) = happyShift action_8
action_0 (80) = happyShift action_9
action_0 (81) = happyShift action_10
action_0 (82) = happyShift action_11
action_0 (83) = happyShift action_12
action_0 (84) = happyShift action_13
action_0 (85) = happyShift action_14
action_0 (86) = happyShift action_15
action_0 (87) = happyShift action_16
action_0 (88) = happyShift action_17
action_0 (89) = happyShift action_18
action_0 (90) = happyShift action_19
action_0 (91) = happyShift action_20
action_0 (92) = happyShift action_21
action_0 (4) = happyGoto action_22
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 _ = happyReduce_2

action_1 (68) = happyShift action_4
action_1 (75) = happyShift action_5
action_1 (76) = happyShift action_6
action_1 (77) = happyShift action_7
action_1 (78) = happyShift action_8
action_1 (80) = happyShift action_9
action_1 (81) = happyShift action_10
action_1 (82) = happyShift action_11
action_1 (83) = happyShift action_12
action_1 (84) = happyShift action_13
action_1 (85) = happyShift action_14
action_1 (86) = happyShift action_15
action_1 (87) = happyShift action_16
action_1 (88) = happyShift action_17
action_1 (89) = happyShift action_18
action_1 (90) = happyShift action_19
action_1 (91) = happyShift action_20
action_1 (92) = happyShift action_21
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (68) = happyShift action_4
action_3 (75) = happyShift action_5
action_3 (76) = happyShift action_6
action_3 (77) = happyShift action_7
action_3 (78) = happyShift action_8
action_3 (80) = happyShift action_9
action_3 (81) = happyShift action_10
action_3 (82) = happyShift action_11
action_3 (83) = happyShift action_12
action_3 (84) = happyShift action_13
action_3 (85) = happyShift action_14
action_3 (86) = happyShift action_15
action_3 (87) = happyShift action_16
action_3 (88) = happyShift action_17
action_3 (89) = happyShift action_18
action_3 (90) = happyShift action_19
action_3 (91) = happyShift action_20
action_3 (92) = happyShift action_21
action_3 (5) = happyGoto action_61
action_3 (6) = happyGoto action_3
action_3 _ = happyReduce_2

action_4 (47) = happyShift action_56
action_4 (75) = happyShift action_57
action_4 (76) = happyShift action_58
action_4 (78) = happyShift action_59
action_4 (81) = happyShift action_60
action_4 (20) = happyGoto action_55
action_4 _ = happyFail

action_5 (47) = happyShift action_53
action_5 (70) = happyShift action_54
action_5 _ = happyFail

action_6 (75) = happyShift action_40
action_6 (17) = happyGoto action_52
action_6 _ = happyFail

action_7 (75) = happyShift action_40
action_7 (17) = happyGoto action_51
action_7 _ = happyFail

action_8 (47) = happyShift action_50
action_8 (15) = happyGoto action_48
action_8 (16) = happyGoto action_49
action_8 _ = happyFail

action_9 (75) = happyShift action_47
action_9 (8) = happyGoto action_45
action_9 (9) = happyGoto action_46
action_9 _ = happyFail

action_10 (75) = happyShift action_40
action_10 (17) = happyGoto action_44
action_10 _ = happyFail

action_11 (47) = happyShift action_43
action_11 (13) = happyGoto action_41
action_11 (14) = happyGoto action_42
action_11 _ = happyFail

action_12 (75) = happyShift action_40
action_12 (17) = happyGoto action_39
action_12 _ = happyFail

action_13 (63) = happyShift action_37
action_13 (75) = happyShift action_38
action_13 (18) = happyGoto action_35
action_13 (19) = happyGoto action_36
action_13 _ = happyFail

action_14 (47) = happyShift action_28
action_14 (56) = happyShift action_29
action_14 (58) = happyShift action_30
action_14 (75) = happyShift action_31
action_14 (38) = happyGoto action_34
action_14 _ = happyFail

action_15 (47) = happyShift action_28
action_15 (56) = happyShift action_29
action_15 (58) = happyShift action_30
action_15 (75) = happyShift action_31
action_15 (38) = happyGoto action_33
action_15 _ = happyFail

action_16 (75) = happyShift action_32
action_16 _ = happyFail

action_17 (47) = happyShift action_28
action_17 (56) = happyShift action_29
action_17 (58) = happyShift action_30
action_17 (75) = happyShift action_31
action_17 (38) = happyGoto action_27
action_17 _ = happyFail

action_18 (75) = happyShift action_26
action_18 _ = happyFail

action_19 (45) = happyShift action_25
action_19 _ = happyFail

action_20 (75) = happyShift action_24
action_20 _ = happyFail

action_21 (75) = happyShift action_23
action_21 _ = happyFail

action_22 (95) = happyAccept
action_22 _ = happyFail

action_23 (70) = happyShift action_118
action_23 _ = happyFail

action_24 (70) = happyShift action_117
action_24 _ = happyFail

action_25 (42) = happyShift action_111
action_25 (47) = happyShift action_112
action_25 (48) = happyShift action_113
action_25 (51) = happyShift action_114
action_25 (56) = happyShift action_115
action_25 (75) = happyShift action_116
action_25 (7) = happyGoto action_110
action_25 _ = happyReduce_24

action_26 (61) = happyShift action_109
action_26 _ = happyFail

action_27 (48) = happyShift action_90
action_27 (52) = happyShift action_91
action_27 (53) = happyShift action_92
action_27 (55) = happyShift action_93
action_27 (57) = happyShift action_94
action_27 (58) = happyShift action_95
action_27 (59) = happyShift action_96
action_27 (60) = happyShift action_97
action_27 (63) = happyShift action_98
action_27 (64) = happyShift action_99
action_27 (67) = happyShift action_100
action_27 (69) = happyShift action_101
action_27 (70) = happyShift action_102
action_27 _ = happyReduce_16

action_28 (47) = happyShift action_28
action_28 (56) = happyShift action_29
action_28 (58) = happyShift action_30
action_28 (75) = happyShift action_31
action_28 (38) = happyGoto action_108
action_28 _ = happyFail

action_29 (47) = happyShift action_28
action_29 (56) = happyShift action_29
action_29 (58) = happyShift action_30
action_29 (75) = happyShift action_31
action_29 (38) = happyGoto action_107
action_29 _ = happyFail

action_30 (47) = happyShift action_28
action_30 (56) = happyShift action_29
action_30 (58) = happyShift action_30
action_30 (75) = happyShift action_31
action_30 (38) = happyGoto action_106
action_30 _ = happyFail

action_31 (47) = happyShift action_105
action_31 _ = happyReduce_129

action_32 (70) = happyShift action_104
action_32 _ = happyFail

action_33 (48) = happyShift action_90
action_33 (52) = happyShift action_91
action_33 (53) = happyShift action_92
action_33 (55) = happyShift action_93
action_33 (57) = happyShift action_94
action_33 (58) = happyShift action_95
action_33 (59) = happyShift action_96
action_33 (60) = happyShift action_97
action_33 (63) = happyShift action_98
action_33 (64) = happyShift action_99
action_33 (67) = happyShift action_100
action_33 (69) = happyShift action_101
action_33 (70) = happyShift action_102
action_33 (72) = happyShift action_103
action_33 _ = happyFail

action_34 (48) = happyShift action_90
action_34 (52) = happyShift action_91
action_34 (53) = happyShift action_92
action_34 (55) = happyShift action_93
action_34 (57) = happyShift action_94
action_34 (58) = happyShift action_95
action_34 (59) = happyShift action_96
action_34 (60) = happyShift action_97
action_34 (63) = happyShift action_98
action_34 (64) = happyShift action_99
action_34 (67) = happyShift action_100
action_34 (69) = happyShift action_101
action_34 (70) = happyShift action_102
action_34 _ = happyReduce_13

action_35 _ = happyReduce_12

action_36 (42) = happyShift action_89
action_36 _ = happyReduce_49

action_37 (63) = happyShift action_37
action_37 (75) = happyShift action_38
action_37 (19) = happyGoto action_88
action_37 _ = happyFail

action_38 (47) = happyShift action_87
action_38 _ = happyReduce_51

action_39 _ = happyReduce_11

action_40 (42) = happyShift action_86
action_40 _ = happyReduce_47

action_41 _ = happyReduce_10

action_42 (42) = happyShift action_85
action_42 _ = happyReduce_41

action_43 (75) = happyShift action_84
action_43 _ = happyFail

action_44 _ = happyReduce_9

action_45 _ = happyReduce_7

action_46 (42) = happyShift action_83
action_46 _ = happyReduce_31

action_47 (61) = happyShift action_82
action_47 _ = happyReduce_33

action_48 _ = happyReduce_6

action_49 (42) = happyShift action_81
action_49 _ = happyReduce_44

action_50 (75) = happyShift action_80
action_50 _ = happyFail

action_51 _ = happyReduce_5

action_52 _ = happyReduce_4

action_53 (75) = happyShift action_79
action_53 (29) = happyGoto action_77
action_53 (30) = happyGoto action_78
action_53 _ = happyFail

action_54 (40) = happyShift action_73
action_54 (47) = happyShift action_74
action_54 (56) = happyShift action_29
action_54 (58) = happyShift action_30
action_54 (63) = happyShift action_75
action_54 (75) = happyShift action_76
action_54 (24) = happyGoto action_70
action_54 (25) = happyGoto action_71
action_54 (38) = happyGoto action_72
action_54 _ = happyFail

action_55 (44) = happyShift action_69
action_55 _ = happyReduce_21

action_56 (47) = happyShift action_56
action_56 (75) = happyShift action_57
action_56 (76) = happyShift action_58
action_56 (78) = happyShift action_59
action_56 (81) = happyShift action_60
action_56 (20) = happyGoto action_68
action_56 _ = happyFail

action_57 (45) = happyShift action_66
action_57 (47) = happyShift action_67
action_57 (22) = happyGoto action_65
action_57 _ = happyReduce_64

action_58 (47) = happyShift action_64
action_58 _ = happyFail

action_59 (47) = happyShift action_63
action_59 _ = happyFail

action_60 (47) = happyShift action_62
action_60 _ = happyFail

action_61 _ = happyReduce_3

action_62 (75) = happyShift action_40
action_62 (17) = happyGoto action_184
action_62 _ = happyFail

action_63 (47) = happyShift action_50
action_63 (15) = happyGoto action_183
action_63 (16) = happyGoto action_49
action_63 _ = happyFail

action_64 (75) = happyShift action_40
action_64 (17) = happyGoto action_182
action_64 _ = happyFail

action_65 _ = happyReduce_54

action_66 (46) = happyShift action_181
action_66 (47) = happyShift action_28
action_66 (56) = happyShift action_29
action_66 (58) = happyShift action_30
action_66 (75) = happyShift action_31
action_66 (37) = happyGoto action_180
action_66 (38) = happyGoto action_136
action_66 _ = happyFail

action_67 (75) = happyShift action_179
action_67 (33) = happyGoto action_177
action_67 (34) = happyGoto action_178
action_67 _ = happyFail

action_68 (44) = happyShift action_69
action_68 (51) = happyShift action_176
action_68 _ = happyFail

action_69 (47) = happyShift action_56
action_69 (75) = happyShift action_57
action_69 (76) = happyShift action_58
action_69 (78) = happyShift action_59
action_69 (81) = happyShift action_60
action_69 (20) = happyGoto action_175
action_69 _ = happyFail

action_70 (54) = happyShift action_174
action_70 _ = happyReduce_23

action_71 _ = happyReduce_85

action_72 (48) = happyShift action_90
action_72 (52) = happyShift action_91
action_72 (53) = happyShift action_92
action_72 (55) = happyShift action_93
action_72 (57) = happyShift action_94
action_72 (58) = happyShift action_95
action_72 (59) = happyShift action_96
action_72 (60) = happyShift action_97
action_72 (63) = happyShift action_98
action_72 (64) = happyShift action_99
action_72 (67) = happyShift action_100
action_72 (69) = happyShift action_101
action_72 (70) = happyShift action_102
action_72 (71) = happyShift action_173
action_72 _ = happyFail

action_73 (47) = happyShift action_172
action_73 _ = happyFail

action_74 (40) = happyShift action_73
action_74 (47) = happyShift action_74
action_74 (56) = happyShift action_29
action_74 (58) = happyShift action_30
action_74 (63) = happyShift action_75
action_74 (75) = happyShift action_76
action_74 (24) = happyGoto action_170
action_74 (25) = happyGoto action_71
action_74 (38) = happyGoto action_171
action_74 _ = happyFail

action_75 (47) = happyShift action_28
action_75 (56) = happyShift action_29
action_75 (58) = happyShift action_30
action_75 (75) = happyShift action_31
action_75 (38) = happyGoto action_169
action_75 _ = happyFail

action_76 (45) = happyShift action_167
action_76 (47) = happyShift action_168
action_76 (48) = happyReduce_129
action_76 (51) = happyReduce_129
action_76 (52) = happyReduce_129
action_76 (53) = happyReduce_129
action_76 (55) = happyReduce_129
action_76 (57) = happyReduce_129
action_76 (58) = happyReduce_129
action_76 (59) = happyReduce_129
action_76 (60) = happyReduce_129
action_76 (63) = happyReduce_129
action_76 (64) = happyReduce_129
action_76 (67) = happyReduce_129
action_76 (69) = happyReduce_129
action_76 (70) = happyReduce_129
action_76 (71) = happyReduce_129
action_76 (72) = happyReduce_129
action_76 (21) = happyGoto action_166
action_76 _ = happyReduce_61

action_77 (42) = happyShift action_164
action_77 (51) = happyShift action_165
action_77 _ = happyFail

action_78 _ = happyReduce_99

action_79 (61) = happyShift action_163
action_79 _ = happyFail

action_80 (42) = happyShift action_162
action_80 _ = happyFail

action_81 (47) = happyShift action_50
action_81 (15) = happyGoto action_161
action_81 (16) = happyGoto action_49
action_81 _ = happyFail

action_82 (49) = happyShift action_159
action_82 (79) = happyShift action_160
action_82 _ = happyFail

action_83 (75) = happyShift action_47
action_83 (8) = happyGoto action_158
action_83 (9) = happyGoto action_46
action_83 _ = happyFail

action_84 (42) = happyShift action_157
action_84 _ = happyFail

action_85 (47) = happyShift action_43
action_85 (13) = happyGoto action_156
action_85 (14) = happyGoto action_42
action_85 _ = happyFail

action_86 (75) = happyShift action_40
action_86 (17) = happyGoto action_155
action_86 _ = happyFail

action_87 (75) = happyShift action_40
action_87 (17) = happyGoto action_154
action_87 _ = happyFail

action_88 (64) = happyShift action_153
action_88 _ = happyFail

action_89 (63) = happyShift action_37
action_89 (75) = happyShift action_38
action_89 (18) = happyGoto action_152
action_89 (19) = happyGoto action_36
action_89 _ = happyFail

action_90 (47) = happyShift action_28
action_90 (56) = happyShift action_29
action_90 (58) = happyShift action_30
action_90 (75) = happyShift action_31
action_90 (38) = happyGoto action_151
action_90 _ = happyFail

action_91 (47) = happyShift action_28
action_91 (56) = happyShift action_29
action_91 (58) = happyShift action_30
action_91 (75) = happyShift action_31
action_91 (38) = happyGoto action_150
action_91 _ = happyFail

action_92 (47) = happyShift action_28
action_92 (56) = happyShift action_29
action_92 (58) = happyShift action_30
action_92 (75) = happyShift action_31
action_92 (38) = happyGoto action_149
action_92 _ = happyFail

action_93 (47) = happyShift action_28
action_93 (56) = happyShift action_29
action_93 (58) = happyShift action_30
action_93 (75) = happyShift action_31
action_93 (38) = happyGoto action_148
action_93 _ = happyFail

action_94 (47) = happyShift action_28
action_94 (56) = happyShift action_29
action_94 (58) = happyShift action_30
action_94 (75) = happyShift action_31
action_94 (38) = happyGoto action_147
action_94 _ = happyFail

action_95 (47) = happyShift action_28
action_95 (56) = happyShift action_29
action_95 (58) = happyShift action_30
action_95 (75) = happyShift action_31
action_95 (38) = happyGoto action_146
action_95 _ = happyFail

action_96 (47) = happyShift action_28
action_96 (56) = happyShift action_29
action_96 (58) = happyShift action_30
action_96 (75) = happyShift action_31
action_96 (38) = happyGoto action_145
action_96 _ = happyFail

action_97 (47) = happyShift action_28
action_97 (56) = happyShift action_29
action_97 (58) = happyShift action_30
action_97 (75) = happyShift action_31
action_97 (38) = happyGoto action_144
action_97 _ = happyFail

action_98 (47) = happyShift action_28
action_98 (56) = happyShift action_29
action_98 (58) = happyShift action_30
action_98 (75) = happyShift action_31
action_98 (38) = happyGoto action_143
action_98 _ = happyFail

action_99 (47) = happyShift action_28
action_99 (56) = happyShift action_29
action_99 (58) = happyShift action_30
action_99 (75) = happyShift action_31
action_99 (38) = happyGoto action_142
action_99 _ = happyFail

action_100 (47) = happyShift action_28
action_100 (56) = happyShift action_29
action_100 (58) = happyShift action_30
action_100 (75) = happyShift action_31
action_100 (38) = happyGoto action_141
action_100 _ = happyFail

action_101 (47) = happyShift action_28
action_101 (56) = happyShift action_29
action_101 (58) = happyShift action_30
action_101 (75) = happyShift action_31
action_101 (38) = happyGoto action_140
action_101 _ = happyFail

action_102 (47) = happyShift action_28
action_102 (56) = happyShift action_29
action_102 (58) = happyShift action_30
action_102 (75) = happyShift action_31
action_102 (38) = happyGoto action_139
action_102 _ = happyFail

action_103 (47) = happyShift action_28
action_103 (56) = happyShift action_29
action_103 (58) = happyShift action_30
action_103 (75) = happyShift action_31
action_103 (38) = happyGoto action_138
action_103 _ = happyFail

action_104 (47) = happyShift action_28
action_104 (56) = happyShift action_29
action_104 (58) = happyShift action_30
action_104 (75) = happyShift action_31
action_104 (38) = happyGoto action_137
action_104 _ = happyFail

action_105 (47) = happyShift action_28
action_105 (56) = happyShift action_29
action_105 (58) = happyShift action_30
action_105 (75) = happyShift action_31
action_105 (37) = happyGoto action_135
action_105 (38) = happyGoto action_136
action_105 _ = happyFail

action_106 _ = happyReduce_125

action_107 (52) = happyShift action_91
action_107 (53) = happyShift action_92
action_107 (57) = happyShift action_94
action_107 (58) = happyShift action_95
action_107 (59) = happyShift action_96
action_107 (60) = happyShift action_97
action_107 (63) = happyShift action_98
action_107 (64) = happyShift action_99
action_107 (67) = happyShift action_100
action_107 (69) = happyShift action_101
action_107 (70) = happyShift action_102
action_107 _ = happyReduce_121

action_108 (48) = happyShift action_90
action_108 (51) = happyShift action_134
action_108 (52) = happyShift action_91
action_108 (53) = happyShift action_92
action_108 (55) = happyShift action_93
action_108 (57) = happyShift action_94
action_108 (58) = happyShift action_95
action_108 (59) = happyShift action_96
action_108 (60) = happyShift action_97
action_108 (63) = happyShift action_98
action_108 (64) = happyShift action_99
action_108 (67) = happyShift action_100
action_108 (69) = happyShift action_101
action_108 (70) = happyShift action_102
action_108 _ = happyFail

action_109 (49) = happyShift action_130
action_109 (75) = happyShift action_131
action_109 (93) = happyShift action_132
action_109 (94) = happyShift action_133
action_109 (26) = happyGoto action_129
action_109 _ = happyFail

action_110 (46) = happyShift action_128
action_110 _ = happyFail

action_111 (42) = happyShift action_111
action_111 (47) = happyShift action_112
action_111 (48) = happyShift action_113
action_111 (51) = happyShift action_114
action_111 (56) = happyShift action_115
action_111 (75) = happyShift action_116
action_111 (7) = happyGoto action_127
action_111 _ = happyReduce_24

action_112 (42) = happyShift action_111
action_112 (47) = happyShift action_112
action_112 (48) = happyShift action_113
action_112 (51) = happyShift action_114
action_112 (56) = happyShift action_115
action_112 (75) = happyShift action_116
action_112 (7) = happyGoto action_126
action_112 _ = happyReduce_24

action_113 (42) = happyShift action_111
action_113 (47) = happyShift action_112
action_113 (48) = happyShift action_113
action_113 (51) = happyShift action_114
action_113 (56) = happyShift action_115
action_113 (75) = happyShift action_116
action_113 (7) = happyGoto action_125
action_113 _ = happyReduce_24

action_114 (42) = happyShift action_111
action_114 (47) = happyShift action_112
action_114 (48) = happyShift action_113
action_114 (51) = happyShift action_114
action_114 (56) = happyShift action_115
action_114 (75) = happyShift action_116
action_114 (7) = happyGoto action_124
action_114 _ = happyReduce_24

action_115 (42) = happyShift action_111
action_115 (47) = happyShift action_112
action_115 (48) = happyShift action_113
action_115 (51) = happyShift action_114
action_115 (56) = happyShift action_115
action_115 (75) = happyShift action_116
action_115 (7) = happyGoto action_123
action_115 _ = happyReduce_24

action_116 (42) = happyShift action_111
action_116 (47) = happyShift action_112
action_116 (48) = happyShift action_113
action_116 (51) = happyShift action_114
action_116 (56) = happyShift action_115
action_116 (75) = happyShift action_116
action_116 (7) = happyGoto action_122
action_116 _ = happyReduce_24

action_117 (47) = happyShift action_120
action_117 (49) = happyShift action_121
action_117 _ = happyFail

action_118 (47) = happyShift action_119
action_118 _ = happyFail

action_119 (75) = happyShift action_224
action_119 (10) = happyGoto action_221
action_119 (11) = happyGoto action_222
action_119 (12) = happyGoto action_223
action_119 _ = happyFail

action_120 (75) = happyShift action_40
action_120 (17) = happyGoto action_220
action_120 _ = happyFail

action_121 (47) = happyShift action_28
action_121 (56) = happyShift action_29
action_121 (58) = happyShift action_30
action_121 (75) = happyShift action_31
action_121 (38) = happyGoto action_219
action_121 _ = happyFail

action_122 _ = happyReduce_25

action_123 _ = happyReduce_27

action_124 _ = happyReduce_29

action_125 _ = happyReduce_26

action_126 _ = happyReduce_28

action_127 _ = happyReduce_30

action_128 _ = happyReduce_15

action_129 (70) = happyShift action_218
action_129 _ = happyFail

action_130 (47) = happyShift action_28
action_130 (56) = happyShift action_29
action_130 (58) = happyShift action_30
action_130 (75) = happyShift action_31
action_130 (38) = happyGoto action_217
action_130 _ = happyFail

action_131 _ = happyReduce_92

action_132 _ = happyReduce_94

action_133 _ = happyReduce_93

action_134 _ = happyReduce_130

action_135 (42) = happyShift action_188
action_135 (51) = happyShift action_216
action_135 _ = happyFail

action_136 (48) = happyShift action_90
action_136 (52) = happyShift action_91
action_136 (53) = happyShift action_92
action_136 (55) = happyShift action_93
action_136 (57) = happyShift action_94
action_136 (58) = happyShift action_95
action_136 (59) = happyShift action_96
action_136 (60) = happyShift action_97
action_136 (63) = happyShift action_98
action_136 (64) = happyShift action_99
action_136 (67) = happyShift action_100
action_136 (69) = happyShift action_101
action_136 (70) = happyShift action_102
action_136 _ = happyReduce_111

action_137 (48) = happyShift action_90
action_137 (52) = happyShift action_91
action_137 (53) = happyShift action_92
action_137 (55) = happyShift action_93
action_137 (57) = happyShift action_94
action_137 (58) = happyShift action_95
action_137 (59) = happyShift action_96
action_137 (60) = happyShift action_97
action_137 (63) = happyShift action_98
action_137 (64) = happyShift action_99
action_137 (67) = happyShift action_100
action_137 (69) = happyShift action_101
action_137 (70) = happyShift action_102
action_137 _ = happyReduce_17

action_138 (48) = happyShift action_90
action_138 (52) = happyShift action_91
action_138 (53) = happyShift action_92
action_138 (55) = happyShift action_93
action_138 (57) = happyShift action_94
action_138 (58) = happyShift action_95
action_138 (59) = happyShift action_96
action_138 (60) = happyShift action_97
action_138 (63) = happyShift action_98
action_138 (64) = happyShift action_99
action_138 (67) = happyShift action_100
action_138 (69) = happyShift action_101
action_138 (70) = happyShift action_102
action_138 _ = happyReduce_14

action_139 (52) = happyShift action_91
action_139 (53) = happyShift action_92
action_139 (57) = happyFail
action_139 (58) = happyShift action_95
action_139 (59) = happyShift action_96
action_139 (60) = happyShift action_97
action_139 (63) = happyShift action_98
action_139 (64) = happyShift action_99
action_139 (67) = happyShift action_100
action_139 (69) = happyShift action_101
action_139 (70) = happyFail
action_139 _ = happyReduce_119

action_140 (52) = happyShift action_91
action_140 (53) = happyShift action_92
action_140 (58) = happyShift action_95
action_140 (59) = happyShift action_96
action_140 (60) = happyShift action_97
action_140 (63) = happyFail
action_140 (64) = happyFail
action_140 (67) = happyFail
action_140 (69) = happyFail
action_140 _ = happyReduce_118

action_141 (52) = happyShift action_91
action_141 (53) = happyShift action_92
action_141 (58) = happyShift action_95
action_141 (59) = happyShift action_96
action_141 (60) = happyShift action_97
action_141 (63) = happyFail
action_141 (64) = happyFail
action_141 (67) = happyFail
action_141 (69) = happyFail
action_141 _ = happyReduce_114

action_142 (52) = happyShift action_91
action_142 (53) = happyShift action_92
action_142 (58) = happyShift action_95
action_142 (59) = happyShift action_96
action_142 (60) = happyShift action_97
action_142 (63) = happyFail
action_142 (64) = happyFail
action_142 (67) = happyFail
action_142 (69) = happyFail
action_142 _ = happyReduce_115

action_143 (52) = happyShift action_91
action_143 (53) = happyShift action_92
action_143 (58) = happyShift action_95
action_143 (59) = happyShift action_96
action_143 (60) = happyShift action_97
action_143 (63) = happyFail
action_143 (64) = happyFail
action_143 (67) = happyFail
action_143 (69) = happyFail
action_143 _ = happyReduce_113

action_144 (53) = happyShift action_92
action_144 _ = happyReduce_126

action_145 (53) = happyShift action_92
action_145 _ = happyReduce_127

action_146 (53) = happyShift action_92
action_146 (59) = happyShift action_96
action_146 (60) = happyShift action_97
action_146 _ = happyReduce_124

action_147 (52) = happyShift action_91
action_147 (53) = happyShift action_92
action_147 (57) = happyFail
action_147 (58) = happyShift action_95
action_147 (59) = happyShift action_96
action_147 (60) = happyShift action_97
action_147 (63) = happyShift action_98
action_147 (64) = happyShift action_99
action_147 (67) = happyShift action_100
action_147 (69) = happyShift action_101
action_147 (70) = happyFail
action_147 _ = happyReduce_120

action_148 (52) = happyShift action_91
action_148 (53) = happyShift action_92
action_148 (57) = happyShift action_94
action_148 (58) = happyShift action_95
action_148 (59) = happyShift action_96
action_148 (60) = happyShift action_97
action_148 (63) = happyShift action_98
action_148 (64) = happyShift action_99
action_148 (67) = happyShift action_100
action_148 (69) = happyShift action_101
action_148 (70) = happyShift action_102
action_148 _ = happyReduce_116

action_149 _ = happyReduce_123

action_150 (53) = happyShift action_92
action_150 (59) = happyShift action_96
action_150 (60) = happyShift action_97
action_150 _ = happyReduce_122

action_151 (52) = happyShift action_91
action_151 (53) = happyShift action_92
action_151 (55) = happyShift action_93
action_151 (57) = happyShift action_94
action_151 (58) = happyShift action_95
action_151 (59) = happyShift action_96
action_151 (60) = happyShift action_97
action_151 (63) = happyShift action_98
action_151 (64) = happyShift action_99
action_151 (67) = happyShift action_100
action_151 (69) = happyShift action_101
action_151 (70) = happyShift action_102
action_151 _ = happyReduce_117

action_152 _ = happyReduce_50

action_153 _ = happyReduce_53

action_154 (51) = happyShift action_215
action_154 _ = happyFail

action_155 _ = happyReduce_48

action_156 _ = happyReduce_42

action_157 (75) = happyShift action_214
action_157 _ = happyFail

action_158 _ = happyReduce_32

action_159 (47) = happyShift action_28
action_159 (56) = happyShift action_29
action_159 (58) = happyShift action_30
action_159 (75) = happyShift action_31
action_159 (38) = happyGoto action_213
action_159 _ = happyFail

action_160 _ = happyReduce_34

action_161 _ = happyReduce_45

action_162 (75) = happyShift action_212
action_162 _ = happyFail

action_163 (49) = happyShift action_130
action_163 (75) = happyShift action_131
action_163 (93) = happyShift action_132
action_163 (94) = happyShift action_133
action_163 (26) = happyGoto action_211
action_163 _ = happyFail

action_164 (75) = happyShift action_79
action_164 (30) = happyGoto action_210
action_164 _ = happyFail

action_165 (70) = happyShift action_209
action_165 _ = happyFail

action_166 (43) = happyShift action_206
action_166 (49) = happyShift action_207
action_166 (66) = happyShift action_208
action_166 (23) = happyGoto action_205
action_166 _ = happyFail

action_167 (46) = happyShift action_203
action_167 (47) = happyShift action_28
action_167 (56) = happyShift action_29
action_167 (58) = happyShift action_30
action_167 (75) = happyShift action_204
action_167 (31) = happyGoto action_200
action_167 (32) = happyGoto action_201
action_167 (37) = happyGoto action_202
action_167 (38) = happyGoto action_136
action_167 _ = happyFail

action_168 (47) = happyShift action_28
action_168 (51) = happyShift action_199
action_168 (56) = happyShift action_29
action_168 (58) = happyShift action_30
action_168 (75) = happyShift action_31
action_168 (37) = happyGoto action_198
action_168 (38) = happyGoto action_136
action_168 _ = happyFail

action_169 (48) = happyShift action_90
action_169 (52) = happyShift action_91
action_169 (53) = happyShift action_92
action_169 (55) = happyShift action_93
action_169 (57) = happyShift action_94
action_169 (58) = happyShift action_95
action_169 (59) = happyShift action_96
action_169 (60) = happyShift action_97
action_169 (63) = happyShift action_98
action_169 (64) = happyShift action_197
action_169 (67) = happyShift action_100
action_169 (69) = happyShift action_101
action_169 (70) = happyShift action_102
action_169 _ = happyFail

action_170 (51) = happyShift action_196
action_170 (54) = happyShift action_174
action_170 _ = happyFail

action_171 (48) = happyShift action_90
action_171 (51) = happyShift action_134
action_171 (52) = happyShift action_91
action_171 (53) = happyShift action_92
action_171 (55) = happyShift action_93
action_171 (57) = happyShift action_94
action_171 (58) = happyShift action_95
action_171 (59) = happyShift action_96
action_171 (60) = happyShift action_97
action_171 (63) = happyShift action_98
action_171 (64) = happyShift action_99
action_171 (67) = happyShift action_100
action_171 (69) = happyShift action_101
action_171 (70) = happyShift action_102
action_171 (71) = happyShift action_173
action_171 _ = happyFail

action_172 (75) = happyShift action_195
action_172 _ = happyFail

action_173 (40) = happyShift action_73
action_173 (47) = happyShift action_74
action_173 (56) = happyShift action_29
action_173 (58) = happyShift action_30
action_173 (63) = happyShift action_75
action_173 (75) = happyShift action_76
action_173 (24) = happyGoto action_194
action_173 (25) = happyGoto action_71
action_173 (38) = happyGoto action_72
action_173 _ = happyFail

action_174 (40) = happyShift action_73
action_174 (47) = happyShift action_74
action_174 (56) = happyShift action_29
action_174 (58) = happyShift action_30
action_174 (63) = happyShift action_75
action_174 (75) = happyShift action_76
action_174 (24) = happyGoto action_193
action_174 (25) = happyGoto action_71
action_174 (38) = happyGoto action_72
action_174 _ = happyFail

action_175 _ = happyReduce_56

action_176 _ = happyReduce_60

action_177 (42) = happyShift action_191
action_177 (51) = happyShift action_192
action_177 _ = happyFail

action_178 _ = happyReduce_105

action_179 (70) = happyShift action_190
action_179 _ = happyFail

action_180 (42) = happyShift action_188
action_180 (46) = happyShift action_189
action_180 _ = happyFail

action_181 _ = happyReduce_65

action_182 (61) = happyShift action_187
action_182 _ = happyFail

action_183 (61) = happyShift action_186
action_183 _ = happyFail

action_184 (61) = happyShift action_185
action_184 _ = happyFail

action_185 (47) = happyShift action_56
action_185 (75) = happyShift action_57
action_185 (76) = happyShift action_58
action_185 (78) = happyShift action_59
action_185 (81) = happyShift action_60
action_185 (20) = happyGoto action_258
action_185 _ = happyFail

action_186 (47) = happyShift action_56
action_186 (75) = happyShift action_57
action_186 (76) = happyShift action_58
action_186 (78) = happyShift action_59
action_186 (81) = happyShift action_60
action_186 (20) = happyGoto action_257
action_186 _ = happyFail

action_187 (47) = happyShift action_56
action_187 (75) = happyShift action_57
action_187 (76) = happyShift action_58
action_187 (78) = happyShift action_59
action_187 (81) = happyShift action_60
action_187 (20) = happyGoto action_256
action_187 _ = happyFail

action_188 (47) = happyShift action_28
action_188 (56) = happyShift action_29
action_188 (58) = happyShift action_30
action_188 (75) = happyShift action_31
action_188 (38) = happyGoto action_255
action_188 _ = happyFail

action_189 _ = happyReduce_66

action_190 (75) = happyShift action_254
action_190 _ = happyFail

action_191 (75) = happyShift action_179
action_191 (34) = happyGoto action_253
action_191 _ = happyFail

action_192 (45) = happyShift action_66
action_192 (22) = happyGoto action_252
action_192 _ = happyReduce_64

action_193 _ = happyReduce_68

action_194 _ = happyReduce_84

action_195 (61) = happyShift action_251
action_195 _ = happyFail

action_196 _ = happyReduce_86

action_197 (43) = happyShift action_250
action_197 (47) = happyShift action_28
action_197 (56) = happyShift action_29
action_197 (58) = happyShift action_30
action_197 (66) = happyShift action_208
action_197 (75) = happyShift action_31
action_197 (23) = happyGoto action_249
action_197 (38) = happyGoto action_142
action_197 _ = happyFail

action_198 (42) = happyShift action_188
action_198 (51) = happyShift action_248
action_198 _ = happyFail

action_199 _ = happyReduce_62

action_200 (42) = happyShift action_246
action_200 (46) = happyShift action_247
action_200 _ = happyFail

action_201 _ = happyReduce_102

action_202 (42) = happyShift action_188
action_202 (46) = happyShift action_245
action_202 _ = happyFail

action_203 _ = happyReduce_91

action_204 (47) = happyShift action_105
action_204 (62) = happyShift action_244
action_204 _ = happyReduce_129

action_205 (43) = happyShift action_243
action_205 _ = happyFail

action_206 (40) = happyShift action_73
action_206 (41) = happyShift action_242
action_206 (47) = happyShift action_74
action_206 (56) = happyShift action_29
action_206 (58) = happyShift action_30
action_206 (63) = happyShift action_75
action_206 (75) = happyShift action_76
action_206 (24) = happyGoto action_241
action_206 (25) = happyGoto action_71
action_206 (38) = happyGoto action_72
action_206 _ = happyFail

action_207 (47) = happyShift action_28
action_207 (56) = happyShift action_29
action_207 (58) = happyShift action_30
action_207 (75) = happyShift action_31
action_207 (35) = happyGoto action_238
action_207 (36) = happyGoto action_239
action_207 (38) = happyGoto action_240
action_207 _ = happyFail

action_208 (47) = happyShift action_28
action_208 (56) = happyShift action_29
action_208 (58) = happyShift action_30
action_208 (75) = happyShift action_31
action_208 (38) = happyGoto action_237
action_208 _ = happyFail

action_209 (40) = happyShift action_73
action_209 (47) = happyShift action_74
action_209 (56) = happyShift action_29
action_209 (58) = happyShift action_30
action_209 (63) = happyShift action_75
action_209 (75) = happyShift action_76
action_209 (24) = happyGoto action_236
action_209 (25) = happyGoto action_71
action_209 (38) = happyGoto action_72
action_209 _ = happyFail

action_210 _ = happyReduce_100

action_211 _ = happyReduce_101

action_212 (51) = happyShift action_235
action_212 _ = happyFail

action_213 (48) = happyShift action_90
action_213 (52) = happyShift action_91
action_213 (53) = happyShift action_92
action_213 (55) = happyShift action_93
action_213 (57) = happyShift action_94
action_213 (58) = happyShift action_95
action_213 (59) = happyShift action_96
action_213 (60) = happyShift action_97
action_213 (63) = happyShift action_98
action_213 (64) = happyShift action_99
action_213 (65) = happyShift action_234
action_213 (67) = happyShift action_100
action_213 (69) = happyShift action_101
action_213 (70) = happyShift action_102
action_213 _ = happyFail

action_214 (42) = happyShift action_233
action_214 _ = happyFail

action_215 _ = happyReduce_52

action_216 _ = happyReduce_128

action_217 (48) = happyShift action_90
action_217 (52) = happyShift action_91
action_217 (53) = happyShift action_92
action_217 (55) = happyShift action_93
action_217 (57) = happyShift action_94
action_217 (58) = happyShift action_95
action_217 (59) = happyShift action_96
action_217 (60) = happyShift action_97
action_217 (63) = happyShift action_98
action_217 (64) = happyShift action_99
action_217 (65) = happyShift action_232
action_217 (67) = happyShift action_100
action_217 (69) = happyShift action_101
action_217 (70) = happyShift action_102
action_217 _ = happyFail

action_218 (47) = happyShift action_28
action_218 (56) = happyShift action_29
action_218 (58) = happyShift action_30
action_218 (75) = happyShift action_31
action_218 (38) = happyGoto action_231
action_218 _ = happyFail

action_219 (48) = happyShift action_90
action_219 (52) = happyShift action_91
action_219 (53) = happyShift action_92
action_219 (55) = happyShift action_93
action_219 (57) = happyShift action_94
action_219 (58) = happyShift action_95
action_219 (59) = happyShift action_96
action_219 (60) = happyShift action_97
action_219 (63) = happyShift action_98
action_219 (64) = happyShift action_99
action_219 (65) = happyShift action_230
action_219 (67) = happyShift action_100
action_219 (69) = happyShift action_101
action_219 (70) = happyShift action_102
action_219 _ = happyFail

action_220 (51) = happyShift action_229
action_220 _ = happyFail

action_221 (51) = happyShift action_228
action_221 _ = happyFail

action_222 (42) = happyShift action_227
action_222 _ = happyReduce_36

action_223 (72) = happyShift action_226
action_223 _ = happyFail

action_224 (60) = happyShift action_225
action_224 _ = happyReduce_39

action_225 (75) = happyShift action_224
action_225 (12) = happyGoto action_280
action_225 _ = happyFail

action_226 (75) = happyShift action_279
action_226 _ = happyFail

action_227 (75) = happyShift action_224
action_227 (10) = happyGoto action_278
action_227 (11) = happyGoto action_222
action_227 (12) = happyGoto action_223
action_227 _ = happyFail

action_228 _ = happyReduce_20

action_229 _ = happyReduce_18

action_230 (47) = happyShift action_28
action_230 (56) = happyShift action_29
action_230 (58) = happyShift action_30
action_230 (75) = happyShift action_31
action_230 (38) = happyGoto action_277
action_230 _ = happyFail

action_231 (48) = happyShift action_90
action_231 (52) = happyShift action_91
action_231 (53) = happyShift action_92
action_231 (55) = happyShift action_93
action_231 (57) = happyShift action_94
action_231 (58) = happyShift action_95
action_231 (59) = happyShift action_96
action_231 (60) = happyShift action_97
action_231 (63) = happyShift action_98
action_231 (64) = happyShift action_99
action_231 (67) = happyShift action_100
action_231 (69) = happyShift action_101
action_231 (70) = happyShift action_102
action_231 _ = happyReduce_8

action_232 (47) = happyShift action_28
action_232 (56) = happyShift action_29
action_232 (58) = happyShift action_30
action_232 (75) = happyShift action_31
action_232 (38) = happyGoto action_276
action_232 _ = happyFail

action_233 (75) = happyShift action_275
action_233 _ = happyFail

action_234 (47) = happyShift action_28
action_234 (56) = happyShift action_29
action_234 (58) = happyShift action_30
action_234 (75) = happyShift action_31
action_234 (38) = happyGoto action_274
action_234 _ = happyFail

action_235 _ = happyReduce_46

action_236 (54) = happyShift action_174
action_236 _ = happyReduce_22

action_237 (48) = happyShift action_90
action_237 (52) = happyShift action_91
action_237 (53) = happyShift action_92
action_237 (55) = happyShift action_93
action_237 (57) = happyShift action_94
action_237 (58) = happyShift action_95
action_237 (59) = happyShift action_96
action_237 (60) = happyShift action_97
action_237 (63) = happyShift action_98
action_237 (64) = happyShift action_99
action_237 (67) = happyShift action_100
action_237 (69) = happyShift action_101
action_237 (70) = happyShift action_102
action_237 _ = happyReduce_67

action_238 (42) = happyShift action_272
action_238 (50) = happyShift action_273
action_238 _ = happyFail

action_239 _ = happyReduce_108

action_240 (48) = happyShift action_90
action_240 (52) = happyShift action_91
action_240 (53) = happyShift action_92
action_240 (55) = happyShift action_93
action_240 (57) = happyShift action_94
action_240 (58) = happyShift action_95
action_240 (59) = happyShift action_96
action_240 (60) = happyShift action_97
action_240 (62) = happyShift action_271
action_240 (63) = happyShift action_98
action_240 (64) = happyShift action_99
action_240 (67) = happyShift action_100
action_240 (69) = happyShift action_101
action_240 (70) = happyShift action_102
action_240 _ = happyFail

action_241 _ = happyReduce_70

action_242 (47) = happyShift action_270
action_242 _ = happyFail

action_243 (40) = happyShift action_73
action_243 (41) = happyShift action_269
action_243 (47) = happyShift action_74
action_243 (56) = happyShift action_29
action_243 (58) = happyShift action_30
action_243 (63) = happyShift action_75
action_243 (75) = happyShift action_76
action_243 (24) = happyGoto action_268
action_243 (25) = happyGoto action_71
action_243 (38) = happyGoto action_72
action_243 _ = happyFail

action_244 (47) = happyShift action_28
action_244 (56) = happyShift action_29
action_244 (58) = happyShift action_30
action_244 (75) = happyShift action_31
action_244 (38) = happyGoto action_267
action_244 _ = happyFail

action_245 _ = happyReduce_89

action_246 (75) = happyShift action_266
action_246 (32) = happyGoto action_265
action_246 _ = happyFail

action_247 _ = happyReduce_90

action_248 (48) = happyReduce_128
action_248 (51) = happyReduce_128
action_248 (52) = happyReduce_128
action_248 (53) = happyReduce_128
action_248 (55) = happyReduce_128
action_248 (57) = happyReduce_128
action_248 (58) = happyReduce_128
action_248 (59) = happyReduce_128
action_248 (60) = happyReduce_128
action_248 (63) = happyReduce_128
action_248 (64) = happyReduce_128
action_248 (67) = happyReduce_128
action_248 (69) = happyReduce_128
action_248 (70) = happyReduce_128
action_248 (71) = happyReduce_128
action_248 (72) = happyReduce_128
action_248 _ = happyReduce_63

action_249 (43) = happyShift action_264
action_249 _ = happyFail

action_250 (40) = happyShift action_73
action_250 (47) = happyShift action_74
action_250 (56) = happyShift action_29
action_250 (58) = happyShift action_30
action_250 (63) = happyShift action_75
action_250 (75) = happyShift action_76
action_250 (24) = happyGoto action_263
action_250 (25) = happyGoto action_71
action_250 (38) = happyGoto action_72
action_250 _ = happyFail

action_251 (49) = happyShift action_130
action_251 (75) = happyShift action_131
action_251 (93) = happyShift action_132
action_251 (94) = happyShift action_133
action_251 (26) = happyGoto action_262
action_251 _ = happyFail

action_252 _ = happyReduce_55

action_253 _ = happyReduce_106

action_254 _ = happyReduce_107

action_255 (48) = happyShift action_90
action_255 (52) = happyShift action_91
action_255 (53) = happyShift action_92
action_255 (55) = happyShift action_93
action_255 (57) = happyShift action_94
action_255 (58) = happyShift action_95
action_255 (59) = happyShift action_96
action_255 (60) = happyShift action_97
action_255 (63) = happyShift action_98
action_255 (64) = happyShift action_99
action_255 (67) = happyShift action_100
action_255 (69) = happyShift action_101
action_255 (70) = happyShift action_102
action_255 _ = happyReduce_112

action_256 (44) = happyShift action_69
action_256 (51) = happyShift action_261
action_256 _ = happyFail

action_257 (44) = happyShift action_69
action_257 (51) = happyShift action_260
action_257 _ = happyFail

action_258 (44) = happyShift action_69
action_258 (51) = happyShift action_259
action_258 _ = happyFail

action_259 _ = happyReduce_58

action_260 _ = happyReduce_59

action_261 _ = happyReduce_57

action_262 (42) = happyShift action_296
action_262 _ = happyFail

action_263 _ = happyReduce_87

action_264 (40) = happyShift action_73
action_264 (47) = happyShift action_74
action_264 (56) = happyShift action_29
action_264 (58) = happyShift action_30
action_264 (63) = happyShift action_75
action_264 (75) = happyShift action_76
action_264 (24) = happyGoto action_295
action_264 (25) = happyGoto action_71
action_264 (38) = happyGoto action_72
action_264 _ = happyFail

action_265 _ = happyReduce_103

action_266 (62) = happyShift action_244
action_266 _ = happyFail

action_267 (48) = happyShift action_90
action_267 (52) = happyShift action_91
action_267 (53) = happyShift action_92
action_267 (55) = happyShift action_93
action_267 (57) = happyShift action_94
action_267 (58) = happyShift action_95
action_267 (59) = happyShift action_96
action_267 (60) = happyShift action_97
action_267 (63) = happyShift action_98
action_267 (64) = happyShift action_99
action_267 (67) = happyShift action_100
action_267 (69) = happyShift action_101
action_267 (70) = happyShift action_102
action_267 _ = happyReduce_104

action_268 _ = happyReduce_69

action_269 (47) = happyShift action_294
action_269 _ = happyFail

action_270 (47) = happyShift action_28
action_270 (56) = happyShift action_29
action_270 (58) = happyShift action_30
action_270 (75) = happyShift action_293
action_270 (27) = happyGoto action_289
action_270 (28) = happyGoto action_290
action_270 (38) = happyGoto action_291
action_270 (39) = happyGoto action_292
action_270 _ = happyFail

action_271 (47) = happyShift action_28
action_271 (56) = happyShift action_29
action_271 (58) = happyShift action_30
action_271 (75) = happyShift action_31
action_271 (38) = happyGoto action_288
action_271 _ = happyFail

action_272 (47) = happyShift action_28
action_272 (56) = happyShift action_29
action_272 (58) = happyShift action_30
action_272 (75) = happyShift action_31
action_272 (36) = happyGoto action_287
action_272 (38) = happyGoto action_240
action_272 _ = happyFail

action_273 (43) = happyShift action_286
action_273 (66) = happyShift action_208
action_273 (23) = happyGoto action_285
action_273 _ = happyFail

action_274 (48) = happyShift action_90
action_274 (50) = happyShift action_284
action_274 (52) = happyShift action_91
action_274 (53) = happyShift action_92
action_274 (55) = happyShift action_93
action_274 (57) = happyShift action_94
action_274 (58) = happyShift action_95
action_274 (59) = happyShift action_96
action_274 (60) = happyShift action_97
action_274 (63) = happyShift action_98
action_274 (64) = happyShift action_99
action_274 (67) = happyShift action_100
action_274 (69) = happyShift action_101
action_274 (70) = happyShift action_102
action_274 _ = happyFail

action_275 (51) = happyShift action_283
action_275 _ = happyFail

action_276 (48) = happyShift action_90
action_276 (50) = happyShift action_282
action_276 (52) = happyShift action_91
action_276 (53) = happyShift action_92
action_276 (55) = happyShift action_93
action_276 (57) = happyShift action_94
action_276 (58) = happyShift action_95
action_276 (59) = happyShift action_96
action_276 (60) = happyShift action_97
action_276 (63) = happyShift action_98
action_276 (64) = happyShift action_99
action_276 (67) = happyShift action_100
action_276 (69) = happyShift action_101
action_276 (70) = happyShift action_102
action_276 _ = happyFail

action_277 (48) = happyShift action_90
action_277 (50) = happyShift action_281
action_277 (52) = happyShift action_91
action_277 (53) = happyShift action_92
action_277 (55) = happyShift action_93
action_277 (57) = happyShift action_94
action_277 (58) = happyShift action_95
action_277 (59) = happyShift action_96
action_277 (60) = happyShift action_97
action_277 (63) = happyShift action_98
action_277 (64) = happyShift action_99
action_277 (67) = happyShift action_100
action_277 (69) = happyShift action_101
action_277 (70) = happyShift action_102
action_277 _ = happyFail

action_278 _ = happyReduce_37

action_279 _ = happyReduce_38

action_280 _ = happyReduce_40

action_281 _ = happyReduce_19

action_282 _ = happyReduce_95

action_283 _ = happyReduce_43

action_284 _ = happyReduce_35

action_285 (43) = happyShift action_307
action_285 _ = happyFail

action_286 (40) = happyShift action_73
action_286 (41) = happyShift action_306
action_286 (47) = happyShift action_74
action_286 (56) = happyShift action_29
action_286 (58) = happyShift action_30
action_286 (63) = happyShift action_75
action_286 (75) = happyShift action_76
action_286 (24) = happyGoto action_305
action_286 (25) = happyGoto action_71
action_286 (38) = happyGoto action_72
action_286 _ = happyFail

action_287 _ = happyReduce_109

action_288 (48) = happyShift action_90
action_288 (52) = happyShift action_91
action_288 (53) = happyShift action_92
action_288 (55) = happyShift action_93
action_288 (57) = happyShift action_94
action_288 (58) = happyShift action_95
action_288 (59) = happyShift action_96
action_288 (60) = happyShift action_97
action_288 (63) = happyShift action_98
action_288 (64) = happyShift action_99
action_288 (67) = happyShift action_100
action_288 (69) = happyShift action_101
action_288 (70) = happyShift action_102
action_288 _ = happyReduce_110

action_289 (61) = happyShift action_304
action_289 _ = happyFail

action_290 (44) = happyShift action_303
action_290 _ = happyReduce_96

action_291 (48) = happyShift action_90
action_291 (52) = happyShift action_91
action_291 (53) = happyShift action_92
action_291 (55) = happyShift action_93
action_291 (57) = happyShift action_94
action_291 (58) = happyShift action_95
action_291 (59) = happyShift action_96
action_291 (60) = happyShift action_97
action_291 (63) = happyShift action_98
action_291 (64) = happyShift action_99
action_291 (67) = happyShift action_100
action_291 (69) = happyShift action_101
action_291 (70) = happyShift action_102
action_291 (72) = happyShift action_302
action_291 _ = happyFail

action_292 (51) = happyShift action_301
action_292 _ = happyFail

action_293 (47) = happyShift action_105
action_293 (61) = happyShift action_300
action_293 _ = happyReduce_129

action_294 (47) = happyShift action_28
action_294 (56) = happyShift action_29
action_294 (58) = happyShift action_30
action_294 (75) = happyShift action_293
action_294 (27) = happyGoto action_298
action_294 (28) = happyGoto action_290
action_294 (38) = happyGoto action_291
action_294 (39) = happyGoto action_299
action_294 _ = happyFail

action_295 _ = happyReduce_88

action_296 (40) = happyShift action_73
action_296 (47) = happyShift action_74
action_296 (56) = happyShift action_29
action_296 (58) = happyShift action_30
action_296 (63) = happyShift action_75
action_296 (75) = happyShift action_76
action_296 (24) = happyGoto action_297
action_296 (25) = happyGoto action_71
action_296 (38) = happyGoto action_72
action_296 _ = happyFail

action_297 (51) = happyShift action_319
action_297 (54) = happyShift action_174
action_297 _ = happyFail

action_298 (61) = happyShift action_318
action_298 _ = happyFail

action_299 (51) = happyShift action_317
action_299 _ = happyFail

action_300 (49) = happyShift action_130
action_300 (75) = happyShift action_131
action_300 (93) = happyShift action_132
action_300 (94) = happyShift action_133
action_300 (26) = happyGoto action_316
action_300 _ = happyFail

action_301 _ = happyReduce_80

action_302 (40) = happyShift action_73
action_302 (47) = happyShift action_74
action_302 (56) = happyShift action_29
action_302 (58) = happyShift action_30
action_302 (63) = happyShift action_75
action_302 (75) = happyShift action_76
action_302 (24) = happyGoto action_315
action_302 (25) = happyGoto action_71
action_302 (38) = happyGoto action_72
action_302 _ = happyFail

action_303 (75) = happyShift action_314
action_303 (27) = happyGoto action_313
action_303 (28) = happyGoto action_290
action_303 _ = happyFail

action_304 (40) = happyShift action_73
action_304 (47) = happyShift action_74
action_304 (49) = happyShift action_312
action_304 (56) = happyShift action_29
action_304 (58) = happyShift action_30
action_304 (63) = happyShift action_75
action_304 (75) = happyShift action_76
action_304 (24) = happyGoto action_311
action_304 (25) = happyGoto action_71
action_304 (38) = happyGoto action_72
action_304 _ = happyFail

action_305 _ = happyReduce_72

action_306 (47) = happyShift action_310
action_306 _ = happyFail

action_307 (40) = happyShift action_73
action_307 (41) = happyShift action_309
action_307 (47) = happyShift action_74
action_307 (56) = happyShift action_29
action_307 (58) = happyShift action_30
action_307 (63) = happyShift action_75
action_307 (75) = happyShift action_76
action_307 (24) = happyGoto action_308
action_307 (25) = happyGoto action_71
action_307 (38) = happyGoto action_72
action_307 _ = happyFail

action_308 _ = happyReduce_71

action_309 (47) = happyShift action_328
action_309 _ = happyFail

action_310 (47) = happyShift action_28
action_310 (56) = happyShift action_29
action_310 (58) = happyShift action_30
action_310 (75) = happyShift action_293
action_310 (27) = happyGoto action_326
action_310 (28) = happyGoto action_290
action_310 (38) = happyGoto action_291
action_310 (39) = happyGoto action_327
action_310 _ = happyFail

action_311 (51) = happyShift action_325
action_311 (54) = happyShift action_174
action_311 _ = happyFail

action_312 (47) = happyShift action_28
action_312 (56) = happyShift action_29
action_312 (58) = happyShift action_30
action_312 (75) = happyShift action_31
action_312 (35) = happyGoto action_324
action_312 (36) = happyGoto action_239
action_312 (38) = happyGoto action_240
action_312 _ = happyFail

action_313 _ = happyReduce_97

action_314 (61) = happyShift action_300
action_314 _ = happyFail

action_315 (54) = happyShift action_323
action_315 _ = happyReduce_131

action_316 (42) = happyShift action_322
action_316 _ = happyFail

action_317 _ = happyReduce_79

action_318 (40) = happyShift action_73
action_318 (47) = happyShift action_74
action_318 (49) = happyShift action_321
action_318 (56) = happyShift action_29
action_318 (58) = happyShift action_30
action_318 (63) = happyShift action_75
action_318 (75) = happyShift action_76
action_318 (24) = happyGoto action_320
action_318 (25) = happyGoto action_71
action_318 (38) = happyGoto action_72
action_318 _ = happyFail

action_319 _ = happyReduce_83

action_320 (51) = happyShift action_338
action_320 (54) = happyShift action_174
action_320 _ = happyFail

action_321 (47) = happyShift action_28
action_321 (56) = happyShift action_29
action_321 (58) = happyShift action_30
action_321 (75) = happyShift action_31
action_321 (35) = happyGoto action_337
action_321 (36) = happyGoto action_239
action_321 (38) = happyGoto action_240
action_321 _ = happyFail

action_322 (47) = happyShift action_28
action_322 (56) = happyShift action_29
action_322 (58) = happyShift action_30
action_322 (75) = happyShift action_31
action_322 (38) = happyGoto action_336
action_322 _ = happyFail

action_323 (40) = happyShift action_73
action_323 (47) = happyShift action_74
action_323 (56) = happyShift action_29
action_323 (58) = happyShift action_30
action_323 (63) = happyShift action_75
action_323 (75) = happyShift action_76
action_323 (24) = happyGoto action_193
action_323 (25) = happyGoto action_71
action_323 (38) = happyGoto action_334
action_323 (39) = happyGoto action_335
action_323 _ = happyFail

action_324 (42) = happyShift action_272
action_324 (50) = happyShift action_333
action_324 _ = happyFail

action_325 _ = happyReduce_74

action_326 (61) = happyShift action_332
action_326 _ = happyFail

action_327 (51) = happyShift action_331
action_327 _ = happyFail

action_328 (47) = happyShift action_28
action_328 (56) = happyShift action_29
action_328 (58) = happyShift action_30
action_328 (75) = happyShift action_293
action_328 (27) = happyGoto action_329
action_328 (28) = happyGoto action_290
action_328 (38) = happyGoto action_291
action_328 (39) = happyGoto action_330
action_328 _ = happyFail

action_329 (61) = happyShift action_343
action_329 _ = happyFail

action_330 (51) = happyShift action_342
action_330 _ = happyFail

action_331 _ = happyReduce_82

action_332 (40) = happyShift action_73
action_332 (47) = happyShift action_74
action_332 (56) = happyShift action_29
action_332 (58) = happyShift action_30
action_332 (63) = happyShift action_75
action_332 (75) = happyShift action_76
action_332 (24) = happyGoto action_341
action_332 (25) = happyGoto action_71
action_332 (38) = happyGoto action_72
action_332 _ = happyFail

action_333 (40) = happyShift action_73
action_333 (47) = happyShift action_74
action_333 (56) = happyShift action_29
action_333 (58) = happyShift action_30
action_333 (63) = happyShift action_75
action_333 (75) = happyShift action_76
action_333 (24) = happyGoto action_340
action_333 (25) = happyGoto action_71
action_333 (38) = happyGoto action_72
action_333 _ = happyFail

action_334 (48) = happyShift action_90
action_334 (52) = happyShift action_91
action_334 (53) = happyShift action_92
action_334 (55) = happyShift action_93
action_334 (57) = happyShift action_94
action_334 (58) = happyShift action_95
action_334 (59) = happyShift action_96
action_334 (60) = happyShift action_97
action_334 (63) = happyShift action_98
action_334 (64) = happyShift action_99
action_334 (67) = happyShift action_100
action_334 (69) = happyShift action_101
action_334 (70) = happyShift action_102
action_334 (71) = happyShift action_173
action_334 (72) = happyShift action_302
action_334 _ = happyFail

action_335 _ = happyReduce_132

action_336 (48) = happyShift action_90
action_336 (52) = happyShift action_91
action_336 (53) = happyShift action_92
action_336 (55) = happyShift action_93
action_336 (57) = happyShift action_94
action_336 (58) = happyShift action_95
action_336 (59) = happyShift action_96
action_336 (60) = happyShift action_97
action_336 (63) = happyShift action_98
action_336 (64) = happyShift action_99
action_336 (67) = happyShift action_100
action_336 (69) = happyShift action_101
action_336 (70) = happyShift action_102
action_336 _ = happyReduce_98

action_337 (42) = happyShift action_272
action_337 (50) = happyShift action_339
action_337 _ = happyFail

action_338 _ = happyReduce_73

action_339 (40) = happyShift action_73
action_339 (47) = happyShift action_74
action_339 (56) = happyShift action_29
action_339 (58) = happyShift action_30
action_339 (63) = happyShift action_75
action_339 (75) = happyShift action_76
action_339 (24) = happyGoto action_347
action_339 (25) = happyGoto action_71
action_339 (38) = happyGoto action_72
action_339 _ = happyFail

action_340 (51) = happyShift action_346
action_340 (54) = happyShift action_174
action_340 _ = happyFail

action_341 (51) = happyShift action_345
action_341 (54) = happyShift action_174
action_341 _ = happyFail

action_342 _ = happyReduce_81

action_343 (40) = happyShift action_73
action_343 (47) = happyShift action_74
action_343 (56) = happyShift action_29
action_343 (58) = happyShift action_30
action_343 (63) = happyShift action_75
action_343 (75) = happyShift action_76
action_343 (24) = happyGoto action_344
action_343 (25) = happyGoto action_71
action_343 (38) = happyGoto action_72
action_343 _ = happyFail

action_344 (51) = happyShift action_349
action_344 (54) = happyShift action_174
action_344 _ = happyFail

action_345 _ = happyReduce_76

action_346 _ = happyReduce_78

action_347 (51) = happyShift action_348
action_347 (54) = happyShift action_174
action_347 _ = happyFail

action_348 _ = happyReduce_77

action_349 _ = happyReduce_75

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  6 happyReduction_4
happyReduction_4 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (DataHiding happy_var_2
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  6 happyReduction_5
happyReduction_5 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (DataParam happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  6 happyReduction_6
happyReduction_6 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (DataRenaming happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  6 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (DataActions happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 6 6 happyReduction_8
happyReduction_8 ((HappyAbsSyn38  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DataGlobal (happy_var_2, happy_var_4) happy_var_6
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_2  6 happyReduction_9
happyReduction_9 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (DataEncapsulation happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  6 happyReduction_10
happyReduction_10 (HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (DataCommunication happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  6 happyReduction_11
happyReduction_11 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (DataNoCommunication happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  6 happyReduction_12
happyReduction_12 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (DataReach happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  6 happyReduction_13
happyReduction_13 (HappyAbsSyn38  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (DataReachCondition happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 4 6 happyReduction_14
happyReduction_14 ((HappyAbsSyn38  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn38  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DataStateReward happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 4 6 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DataUntilFormula happy_var_3
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_2  6 happyReduction_16
happyReduction_16 (HappyAbsSyn38  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (DataFormula happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happyReduce 4 6 happyReduction_17
happyReduction_17 ((HappyAbsSyn38  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DataConstant happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 6 6 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DataEnumType happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 8 6 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn38  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn38  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DataRangeType happy_var_2 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 6 6 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DataFunction happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_2  6 happyReduction_21
happyReduction_21 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (ParserInitialProcess happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 6 6 happyReduction_22
happyReduction_22 ((HappyAbsSyn24  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (ParserProcess (Process happy_var_1 (reverse happy_var_3) happy_var_6)
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_3  6 happyReduction_23
happyReduction_23 (HappyAbsSyn24  happy_var_3)
	_
	(HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn6
		 (ParserProcess (Process happy_var_1 [] happy_var_3)
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_0  7 happyReduction_24
happyReduction_24  =  HappyAbsSyn7
		 (""
	)

happyReduce_25 = happySpecReduce_2  7 happyReduction_25
happyReduction_25 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn7
		 (happy_var_1 ++ " " ++ happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  7 happyReduction_26
happyReduction_26 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 ("|" ++ " " ++ happy_var_2
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_2  7 happyReduction_27
happyReduction_27 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 ("!" ++ " " ++ happy_var_2
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  7 happyReduction_28
happyReduction_28 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 ("(" ++ " " ++ happy_var_2
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  7 happyReduction_29
happyReduction_29 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (")" ++ " " ++ happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_2  7 happyReduction_30
happyReduction_30 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 ("," ++ " " ++ happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  8 happyReduction_31
happyReduction_31 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  8 happyReduction_32
happyReduction_32 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1:happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  9 happyReduction_33
happyReduction_33 (HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn9
		 ((happy_var_1, None)
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  9 happyReduction_34
happyReduction_34 _
	_
	(HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn9
		 ((happy_var_1, Bool)
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happyReduce 7 9 happyReduction_35
happyReduction_35 (_ `HappyStk`
	(HappyAbsSyn38  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn38  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((happy_var_1, IntRange happy_var_4 happy_var_6)
	) `HappyStk` happyRest

happyReduce_36 = happySpecReduce_1  10 happyReduction_36
happyReduction_36 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  10 happyReduction_37
happyReduction_37 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1:happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  11 happyReduction_38
happyReduction_38 (HappyTerminal (TokenString happy_var_3))
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ((happy_var_1, happy_var_3)
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  12 happyReduction_39
happyReduction_39 (HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  12 happyReduction_40
happyReduction_40 (HappyAbsSyn12  happy_var_3)
	_
	(HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn12
		 (happy_var_1:happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  13 happyReduction_41
happyReduction_41 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  13 happyReduction_42
happyReduction_42 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happyReduce 7 14 happyReduction_43
happyReduction_43 (_ `HappyStk`
	(HappyTerminal (TokenString happy_var_6)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 ((happy_var_2,happy_var_4,happy_var_6)
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_1  15 happyReduction_44
happyReduction_44 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  15 happyReduction_45
happyReduction_45 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 : happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happyReduce 5 16 happyReduction_46
happyReduction_46 (_ `HappyStk`
	(HappyTerminal (TokenString happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 ((happy_var_2,happy_var_4)
	) `HappyStk` happyRest

happyReduce_47 = happySpecReduce_1  17 happyReduction_47
happyReduction_47 (HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn17
		 ([happy_var_1]
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  17 happyReduction_48
happyReduction_48 (HappyAbsSyn17  happy_var_3)
	_
	(HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn17
		 (happy_var_1 : happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  18 happyReduction_49
happyReduction_49 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 ([happy_var_1]
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  18 happyReduction_50
happyReduction_50 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 : happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  19 happyReduction_51
happyReduction_51 (HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happyReduce 4 19 happyReduction_52
happyReduction_52 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (happy_var_1 ++ commaList happy_var_3
	) `HappyStk` happyRest

happyReduce_53 = happySpecReduce_3  19 happyReduction_53
happyReduction_53 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 ("rate(" ++ happy_var_2 ++ ")"
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_2  20 happyReduction_54
happyReduction_54 (HappyAbsSyn22  happy_var_2)
	(HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn20
		 (InitSingleProcess (InitialProcess happy_var_1 happy_var_2 [])
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happyReduce 5 20 happyReduction_55
happyReduction_55 ((HappyAbsSyn22  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (InitSingleProcess (InitialProcess happy_var_1 happy_var_5 happy_var_3)
	) `HappyStk` happyRest

happyReduce_56 = happySpecReduce_3  20 happyReduction_56
happyReduction_56 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (InitParallel happy_var_1 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happyReduce 6 20 happyReduction_57
happyReduction_57 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (InitHiding happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_58 = happyReduce 6 20 happyReduction_58
happyReduction_58 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (InitEncapsulation happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_59 = happyReduce 6 20 happyReduction_59
happyReduction_59 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (InitRenaming happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_60 = happySpecReduce_3  20 happyReduction_60
happyReduction_60 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_0  21 happyReduction_61
happyReduction_61  =  HappyAbsSyn21
		 ([]
	)

happyReduce_62 = happySpecReduce_2  21 happyReduction_62
happyReduction_62 _
	_
	 =  HappyAbsSyn21
		 ([]
	)

happyReduce_63 = happySpecReduce_3  21 happyReduction_63
happyReduction_63 _
	(HappyAbsSyn37  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (reverse happy_var_2
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_0  22 happyReduction_64
happyReduction_64  =  HappyAbsSyn22
		 ([]
	)

happyReduce_65 = happySpecReduce_2  22 happyReduction_65
happyReduction_65 _
	_
	 =  HappyAbsSyn22
		 ([]
	)

happyReduce_66 = happySpecReduce_3  22 happyReduction_66
happyReduction_66 _
	(HappyAbsSyn37  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (reverse happy_var_2
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_2  23 happyReduction_67
happyReduction_67 (HappyAbsSyn38  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (happy_var_2
	)
happyReduction_67 _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  24 happyReduction_68
happyReduction_68 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (Plus ([happy_var_1] ++ [happy_var_3])
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happyReduce 5 24 happyReduction_69
happyReduction_69 ((HappyAbsSyn24  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (if (take 4 happy_var_1) /= "rate" then ActionPrefix happy_var_3 happy_var_1 happy_var_2 [("i0", TypeRange 1 1, (Variable "1"))] happy_var_5 else error("Error: you cannot use action names starting with 'rate'")
	) `HappyStk` happyRest

happyReduce_70 = happyReduce 4 24 happyReduction_70
happyReduction_70 ((HappyAbsSyn24  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (if (take 4 happy_var_1) /= "rate" then ActionPrefix (Variable "0") happy_var_1 happy_var_2 [("i0", TypeRange 1 1, (Variable "1"))] happy_var_4 else error("Error: you cannot use action names starting with 'rate'")
	) `HappyStk` happyRest

happyReduce_71 = happyReduce 8 24 happyReduction_71
happyReduction_71 ((HappyAbsSyn24  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (if (take 4 happy_var_1) /= "rate" then ActionPrefix happy_var_6 (happy_var_1  ++ "{" ++ (printExpressions happy_var_4) ++ "}") happy_var_2 [("i0", TypeRange 1 1, (Variable "1"))] happy_var_8 else error("Error: you cannot use action names starting with 'rate'")
	) `HappyStk` happyRest

happyReduce_72 = happyReduce 7 24 happyReduction_72
happyReduction_72 ((HappyAbsSyn24  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (if (take 4 happy_var_1) /= "rate" then ActionPrefix (Variable "0") (happy_var_1  ++ "{" ++ (printExpressions happy_var_4) ++ "}") happy_var_2 [("i0", TypeRange 1 1, (Variable "1"))] happy_var_7 else error("Error: you cannot use action names starting with 'rate'")
	) `HappyStk` happyRest

happyReduce_73 = happyReduce 10 24 happyReduction_73
happyReduction_73 (_ `HappyStk`
	(HappyAbsSyn24  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (if (take 4 happy_var_1) /= "rate" then ActionPrefix happy_var_3 happy_var_1 happy_var_2 happy_var_7 happy_var_9 else error("Error: you cannot use action names starting with 'rate'")
	) `HappyStk` happyRest

happyReduce_74 = happyReduce 9 24 happyReduction_74
happyReduction_74 (_ `HappyStk`
	(HappyAbsSyn24  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (if (take 4 happy_var_1) /= "rate" then ActionPrefix (Variable "0") happy_var_1 happy_var_2 happy_var_6 happy_var_8 else error("Error: you cannot use action names starting with 'rate'")
	) `HappyStk` happyRest

happyReduce_75 = happyReduce 13 24 happyReduction_75
happyReduction_75 (_ `HappyStk`
	(HappyAbsSyn24  happy_var_12) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (if (take 4 happy_var_1) /= "rate" then  ActionPrefix happy_var_6 (happy_var_1  ++ "{" ++ (printExpressions happy_var_4) ++ "}") happy_var_2 happy_var_10 happy_var_12 else error("Error: you cannot use action names starting with 'rate'")
	) `HappyStk` happyRest

happyReduce_76 = happyReduce 12 24 happyReduction_76
happyReduction_76 (_ `HappyStk`
	(HappyAbsSyn24  happy_var_11) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (if (take 4 happy_var_1) /= "rate" then  ActionPrefix (Variable "0") (happy_var_1  ++ "{" ++ (printExpressions happy_var_4) ++ "}") happy_var_2 happy_var_9 happy_var_11 else error("Error: you cannot use action names starting with 'rate'")
	) `HappyStk` happyRest

happyReduce_77 = happyReduce 13 24 happyReduction_77
happyReduction_77 (_ `HappyStk`
	(HappyAbsSyn24  happy_var_12) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (if (take 4 happy_var_1) /= "rate" then  ActionPrefix happy_var_3 (happy_var_1  ++ "{" ++ (printExpressions happy_var_10) ++ "}") happy_var_2 happy_var_7 happy_var_12 else error("Error: you cannot use action names starting with 'rate'")
	) `HappyStk` happyRest

happyReduce_78 = happyReduce 12 24 happyReduction_78
happyReduction_78 (_ `HappyStk`
	(HappyAbsSyn24  happy_var_11) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (if (take 4 happy_var_1) /= "rate" then  ActionPrefix (Variable "0") (happy_var_1  ++ "{" ++ (printExpressions happy_var_9) ++ "}") happy_var_2 happy_var_6 happy_var_11 else error("Error: you cannot use action names starting with 'rate'")
	) `HappyStk` happyRest

happyReduce_79 = happyReduce 8 24 happyReduction_79
happyReduction_79 (_ `HappyStk`
	(HappyAbsSyn39  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (if (take 4 happy_var_1) /= "rate" then  ActionPrefix2 happy_var_3 happy_var_1 happy_var_2 happy_var_7 else error("Error: you cannot use action names starting with 'rate'")
	) `HappyStk` happyRest

happyReduce_80 = happyReduce 7 24 happyReduction_80
happyReduction_80 (_ `HappyStk`
	(HappyAbsSyn39  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (if (take 4 happy_var_1) /= "rate" then  ActionPrefix2 (Variable "0") happy_var_1 happy_var_2 happy_var_6 else error("Error: you cannot use action names starting with 'rate'")
	) `HappyStk` happyRest

happyReduce_81 = happyReduce 11 24 happyReduction_81
happyReduction_81 (_ `HappyStk`
	(HappyAbsSyn39  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (if (take 4 happy_var_1) /= "rate" then  ActionPrefix2 happy_var_6 (happy_var_1  ++ "{" ++ (printExpressions happy_var_4) ++ "}") happy_var_2 happy_var_10 else error("Error: you cannot use action names starting with 'rate'")
	) `HappyStk` happyRest

happyReduce_82 = happyReduce 10 24 happyReduction_82
happyReduction_82 (_ `HappyStk`
	(HappyAbsSyn39  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (if (take 4 happy_var_1) /= "rate" then  ActionPrefix2 (Variable "0") (happy_var_1  ++ "{" ++ (printExpressions happy_var_4) ++ "}") happy_var_2 happy_var_9 else error("Error: you cannot use action names starting with 'rate'")
	) `HappyStk` happyRest

happyReduce_83 = happyReduce 8 24 happyReduction_83
happyReduction_83 (_ `HappyStk`
	(HappyAbsSyn24  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (Sum happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_84 = happySpecReduce_3  24 happyReduction_84
happyReduction_84 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn24
		 (Implication happy_var_1 happy_var_3
	)
happyReduction_84 _ _ _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_1  24 happyReduction_85
happyReduction_85 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_85 _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_3  24 happyReduction_86
happyReduction_86 _
	(HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (happy_var_2
	)
happyReduction_86 _ _ _  = notHappyAtAll 

happyReduce_87 = happyReduce 5 24 happyReduction_87
happyReduction_87 ((HappyAbsSyn24  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn38  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (LambdaPrefix (Variable "0") happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_88 = happyReduce 6 24 happyReduction_88
happyReduction_88 ((HappyAbsSyn24  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn38  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (LambdaPrefix happy_var_4 happy_var_2 happy_var_6
	) `HappyStk` happyRest

happyReduce_89 = happyReduce 4 25 happyReduction_89
happyReduction_89 (_ `HappyStk`
	(HappyAbsSyn37  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (ProcessInstance happy_var_1 (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_90 = happyReduce 4 25 happyReduction_90
happyReduction_90 (_ `HappyStk`
	(HappyAbsSyn31  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (ProcessInstance2 happy_var_1 (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_91 = happySpecReduce_3  25 happyReduction_91
happyReduction_91 _
	_
	(HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn25
		 (ProcessInstance2 happy_var_1 []
	)
happyReduction_91 _ _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_1  26 happyReduction_92
happyReduction_92 (HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn26
		 (TypeName happy_var_1
	)
happyReduction_92 _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_1  26 happyReduction_93
happyReduction_93 _
	 =  HappyAbsSyn26
		 (TypeName "Queue"
	)

happyReduce_94 = happySpecReduce_1  26 happyReduction_94
happyReduction_94 _
	 =  HappyAbsSyn26
		 (TypeName "Nat"
	)

happyReduce_95 = happyReduce 5 26 happyReduction_95
happyReduction_95 (_ `HappyStk`
	(HappyAbsSyn38  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn38  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (TypeRangeExpressions happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_96 = happySpecReduce_1  27 happyReduction_96
happyReduction_96 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn27
		 ([happy_var_1]
	)
happyReduction_96 _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_3  27 happyReduction_97
happyReduction_97 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1 : happy_var_3
	)
happyReduction_97 _ _ _  = notHappyAtAll 

happyReduce_98 = happyReduce 5 28 happyReduction_98
happyReduction_98 ((HappyAbsSyn38  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 ((happy_var_1, happy_var_3, happy_var_5)
	) `HappyStk` happyRest

happyReduce_99 = happySpecReduce_1  29 happyReduction_99
happyReduction_99 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn29
		 ([happy_var_1]
	)
happyReduction_99 _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_3  29 happyReduction_100
happyReduction_100 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_3 : happy_var_1
	)
happyReduction_100 _ _ _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_3  30 happyReduction_101
happyReduction_101 (HappyAbsSyn26  happy_var_3)
	_
	(HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn30
		 ((happy_var_1, happy_var_3)
	)
happyReduction_101 _ _ _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_1  31 happyReduction_102
happyReduction_102 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn31
		 ([happy_var_1]
	)
happyReduction_102 _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_3  31 happyReduction_103
happyReduction_103 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_3 : happy_var_1
	)
happyReduction_103 _ _ _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_3  32 happyReduction_104
happyReduction_104 (HappyAbsSyn38  happy_var_3)
	_
	(HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn32
		 ((happy_var_1, happy_var_3)
	)
happyReduction_104 _ _ _  = notHappyAtAll 

happyReduce_105 = happySpecReduce_1  33 happyReduction_105
happyReduction_105 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn33
		 ([happy_var_1]
	)
happyReduction_105 _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_3  33 happyReduction_106
happyReduction_106 (HappyAbsSyn34  happy_var_3)
	_
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_3 : happy_var_1
	)
happyReduction_106 _ _ _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_3  34 happyReduction_107
happyReduction_107 (HappyTerminal (TokenString happy_var_3))
	_
	(HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn34
		 ((happy_var_1, happy_var_3)
	)
happyReduction_107 _ _ _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_1  35 happyReduction_108
happyReduction_108 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn35
		 ([happy_var_1]
	)
happyReduction_108 _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_3  35 happyReduction_109
happyReduction_109 (HappyAbsSyn36  happy_var_3)
	_
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_3 : happy_var_1
	)
happyReduction_109 _ _ _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_3  36 happyReduction_110
happyReduction_110 (HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn36
		 ((happy_var_1, happy_var_3)
	)
happyReduction_110 _ _ _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_1  37 happyReduction_111
happyReduction_111 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn37
		 ([happy_var_1]
	)
happyReduction_111 _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_3  37 happyReduction_112
happyReduction_112 (HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_3 : happy_var_1
	)
happyReduction_112 _ _ _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_3  38 happyReduction_113
happyReduction_113 (HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (Function "lt" [happy_var_1, happy_var_3]
	)
happyReduction_113 _ _ _  = notHappyAtAll 

happyReduce_114 = happySpecReduce_3  38 happyReduction_114
happyReduction_114 (HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (Function "leq" [happy_var_1, happy_var_3]
	)
happyReduction_114 _ _ _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_3  38 happyReduction_115
happyReduction_115 (HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (Function "gt" [happy_var_1, happy_var_3]
	)
happyReduction_115 _ _ _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_3  38 happyReduction_116
happyReduction_116 (HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (Function "and" [happy_var_1, happy_var_3]
	)
happyReduction_116 _ _ _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_3  38 happyReduction_117
happyReduction_117 (HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (Function "or" [happy_var_1, happy_var_3]
	)
happyReduction_117 _ _ _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_3  38 happyReduction_118
happyReduction_118 (HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (Function "geq" [happy_var_1, happy_var_3]
	)
happyReduction_118 _ _ _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_3  38 happyReduction_119
happyReduction_119 (HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (Function "eq" [happy_var_1, happy_var_3]
	)
happyReduction_119 _ _ _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_3  38 happyReduction_120
happyReduction_120 (HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (Function "not" [Function "eq" [happy_var_1, happy_var_3]]
	)
happyReduction_120 _ _ _  = notHappyAtAll 

happyReduce_121 = happySpecReduce_2  38 happyReduction_121
happyReduction_121 (HappyAbsSyn38  happy_var_2)
	_
	 =  HappyAbsSyn38
		 (Function "not" [happy_var_2]
	)
happyReduction_121 _ _  = notHappyAtAll 

happyReduce_122 = happySpecReduce_3  38 happyReduction_122
happyReduction_122 (HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (Function "plus" [happy_var_1, happy_var_3]
	)
happyReduction_122 _ _ _  = notHappyAtAll 

happyReduce_123 = happySpecReduce_3  38 happyReduction_123
happyReduction_123 (HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (Function "power" [happy_var_1, happy_var_3]
	)
happyReduction_123 _ _ _  = notHappyAtAll 

happyReduce_124 = happySpecReduce_3  38 happyReduction_124
happyReduction_124 (HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (Function "minus" [happy_var_1, happy_var_3]
	)
happyReduction_124 _ _ _  = notHappyAtAll 

happyReduce_125 = happySpecReduce_2  38 happyReduction_125
happyReduction_125 (HappyAbsSyn38  happy_var_2)
	_
	 =  HappyAbsSyn38
		 (Function "multiply" [Variable "-1", happy_var_2]
	)
happyReduction_125 _ _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_3  38 happyReduction_126
happyReduction_126 (HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (Function "multiply" [happy_var_1, happy_var_3]
	)
happyReduction_126 _ _ _  = notHappyAtAll 

happyReduce_127 = happySpecReduce_3  38 happyReduction_127
happyReduction_127 (HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (Function "divide" [happy_var_1, happy_var_3]
	)
happyReduction_127 _ _ _  = notHappyAtAll 

happyReduce_128 = happyReduce 4 38 happyReduction_128
happyReduction_128 (_ `HappyStk`
	(HappyAbsSyn37  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn38
		 (Function happy_var_1 (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_129 = happySpecReduce_1  38 happyReduction_129
happyReduction_129 (HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn38
		 (Variable happy_var_1
	)
happyReduction_129 _  = notHappyAtAll 

happyReduce_130 = happySpecReduce_3  38 happyReduction_130
happyReduction_130 _
	(HappyAbsSyn38  happy_var_2)
	_
	 =  HappyAbsSyn38
		 (happy_var_2
	)
happyReduction_130 _ _ _  = notHappyAtAll 

happyReduce_131 = happySpecReduce_3  39 happyReduction_131
happyReduction_131 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn39
		 ([(happy_var_1, happy_var_3)]
	)
happyReduction_131 _ _ _  = notHappyAtAll 

happyReduce_132 = happyReduce 5 39 happyReduction_132
happyReduction_132 ((HappyAbsSyn39  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn38  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 ((happy_var_1, happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TokenEOF -> action 95 95 tk (HappyState action) sts stk;
	TokenSum -> cont 40;
	TokenPSum -> cont 41;
	TokenComma -> cont 42;
	TokenSeq -> cont 43;
	TokenIndependence -> cont 44;
	TokenOSB -> cont 45;
	TokenCSB -> cont 46;
	TokenOB -> cont 47;
	TokenOr -> cont 48;
	TokenOA -> cont 49;
	TokenCA -> cont 50;
	TokenCB -> cont 51;
	TokenPlus -> cont 52;
	TokenPower -> cont 53;
	TokenPlusPlus -> cont 54;
	TokenAnd -> cont 55;
	TokenNot -> cont 56;
	TokenNotEqual -> cont 57;
	TokenMinus -> cont 58;
	TokenDivide -> cont 59;
	TokenMultiply -> cont 60;
	TokenColon -> cont 61;
	TokenBecomes -> cont 62;
	TokenSmaller -> cont 63;
	TokenGreater -> cont 64;
	TokenDotDot -> cont 65;
	TokenAt -> cont 66;
	TokenSmallerEq -> cont 67;
	TokenInit -> cont 68;
	TokenGreaterEq -> cont 69;
	TokenEqual -> cont 70;
	TokenImplies -> cont 71;
	TokenProbDef -> cont 72;
	TokenTrue -> cont 73;
	TokenFalse -> cont 74;
	TokenString happy_dollar_dollar -> cont 75;
	TokenHide -> cont 76;
	TokenParam -> cont 77;
	TokenRename -> cont 78;
	TokenBool -> cont 79;
	TokenActions -> cont 80;
	TokenEncap -> cont 81;
	TokenComm -> cont 82;
	TokenNoComm -> cont 83;
	TokenReach -> cont 84;
	TokenReachCondition -> cont 85;
	TokenStateReward -> cont 86;
	TokenConstant -> cont 87;
	TokenFormula -> cont 88;
	TokenGlobal -> cont 89;
	TokenUntilFormula -> cont 90;
	TokenType -> cont 91;
	TokenFunction -> cont 92;
	TokenNat -> cont 93;
	TokenQueue -> cont 94;
	_ -> happyError' tk
	})

happyError_ 95 tk = happyError' tk
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

parse = happySomeParser where
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
