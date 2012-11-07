{-# OPTIONS_GHC -w #-}
module Parser where
import Scanner

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16
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

action_0 (49) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (49) = happyShift action_2
action_1 _ = happyFail

action_2 (53) = happyShift action_7
action_2 (5) = happyGoto action_4
action_2 (6) = happyGoto action_5
action_2 (7) = happyGoto action_6
action_2 _ = happyReduce_2

action_3 (56) = happyAccept
action_3 _ = happyFail

action_4 (39) = happyShift action_12
action_4 (43) = happyShift action_13
action_4 (46) = happyShift action_14
action_4 (47) = happyShift action_15
action_4 (48) = happyShift action_16
action_4 (55) = happyShift action_17
action_4 (8) = happyGoto action_10
action_4 (9) = happyGoto action_11
action_4 _ = happyFail

action_5 _ = happyReduce_3

action_6 (53) = happyShift action_7
action_6 (6) = happyGoto action_9
action_6 (7) = happyGoto action_6
action_6 _ = happyReduce_4

action_7 (55) = happyShift action_8
action_7 _ = happyFail

action_8 (20) = happyShift action_41
action_8 (36) = happyShift action_42
action_8 _ = happyFail

action_9 _ = happyReduce_5

action_10 (50) = happyShift action_40
action_10 _ = happyFail

action_11 (39) = happyShift action_12
action_11 (43) = happyShift action_13
action_11 (46) = happyShift action_14
action_11 (47) = happyShift action_15
action_11 (48) = happyShift action_16
action_11 (55) = happyShift action_17
action_11 (8) = happyGoto action_39
action_11 (9) = happyGoto action_11
action_11 _ = happyReduce_8

action_12 (28) = happyShift action_25
action_12 (31) = happyShift action_34
action_12 (32) = happyShift action_35
action_12 (51) = happyShift action_36
action_12 (52) = happyShift action_37
action_12 (54) = happyShift action_27
action_12 (55) = happyShift action_28
action_12 (10) = happyGoto action_30
action_12 (11) = happyGoto action_22
action_12 (12) = happyGoto action_23
action_12 (13) = happyGoto action_24
action_12 (14) = happyGoto action_38
action_12 (15) = happyGoto action_32
action_12 (16) = happyGoto action_33
action_12 _ = happyFail

action_13 (28) = happyShift action_25
action_13 (31) = happyShift action_34
action_13 (32) = happyShift action_35
action_13 (51) = happyShift action_36
action_13 (52) = happyShift action_37
action_13 (54) = happyShift action_27
action_13 (55) = happyShift action_28
action_13 (10) = happyGoto action_30
action_13 (11) = happyGoto action_22
action_13 (12) = happyGoto action_23
action_13 (13) = happyGoto action_24
action_13 (14) = happyGoto action_31
action_13 (15) = happyGoto action_32
action_13 (16) = happyGoto action_33
action_13 _ = happyFail

action_14 (20) = happyShift action_29
action_14 _ = happyFail

action_15 (28) = happyShift action_25
action_15 (32) = happyShift action_26
action_15 (54) = happyShift action_27
action_15 (55) = happyShift action_28
action_15 (10) = happyGoto action_21
action_15 (11) = happyGoto action_22
action_15 (12) = happyGoto action_23
action_15 (13) = happyGoto action_24
action_15 _ = happyFail

action_16 (55) = happyShift action_20
action_16 _ = happyFail

action_17 (19) = happyShift action_18
action_17 (36) = happyShift action_19
action_17 _ = happyFail

action_18 (28) = happyShift action_25
action_18 (32) = happyShift action_26
action_18 (54) = happyShift action_27
action_18 (55) = happyShift action_28
action_18 (10) = happyGoto action_68
action_18 (11) = happyGoto action_22
action_18 (12) = happyGoto action_23
action_18 (13) = happyGoto action_24
action_18 _ = happyFail

action_19 (28) = happyShift action_25
action_19 (32) = happyShift action_26
action_19 (54) = happyShift action_27
action_19 (55) = happyShift action_28
action_19 (10) = happyGoto action_67
action_19 (11) = happyGoto action_22
action_19 (12) = happyGoto action_23
action_19 (13) = happyGoto action_24
action_19 _ = happyFail

action_20 (20) = happyShift action_65
action_20 (36) = happyShift action_66
action_20 _ = happyFail

action_21 (20) = happyShift action_64
action_21 (27) = happyShift action_57
action_21 (28) = happyShift action_58
action_21 _ = happyFail

action_22 (29) = happyShift action_62
action_22 (30) = happyShift action_63
action_22 _ = happyReduce_18

action_23 _ = happyReduce_21

action_24 _ = happyReduce_25

action_25 (32) = happyShift action_26
action_25 (54) = happyShift action_27
action_25 (55) = happyShift action_28
action_25 (13) = happyGoto action_61
action_25 _ = happyFail

action_26 (28) = happyShift action_25
action_26 (32) = happyShift action_26
action_26 (54) = happyShift action_27
action_26 (55) = happyShift action_28
action_26 (10) = happyGoto action_60
action_26 (11) = happyGoto action_22
action_26 (12) = happyGoto action_23
action_26 (13) = happyGoto action_24
action_26 _ = happyFail

action_27 _ = happyReduce_28

action_28 (36) = happyShift action_59
action_28 _ = happyReduce_26

action_29 _ = happyReduce_12

action_30 (21) = happyShift action_51
action_30 (22) = happyShift action_52
action_30 (23) = happyShift action_53
action_30 (24) = happyShift action_54
action_30 (25) = happyShift action_55
action_30 (26) = happyShift action_56
action_30 (27) = happyShift action_57
action_30 (28) = happyShift action_58
action_30 _ = happyFail

action_31 (18) = happyShift action_44
action_31 (44) = happyShift action_50
action_31 _ = happyFail

action_32 (17) = happyShift action_49
action_32 _ = happyReduce_30

action_33 _ = happyReduce_32

action_34 (28) = happyShift action_25
action_34 (31) = happyShift action_34
action_34 (32) = happyShift action_35
action_34 (51) = happyShift action_36
action_34 (52) = happyShift action_37
action_34 (54) = happyShift action_27
action_34 (55) = happyShift action_28
action_34 (10) = happyGoto action_30
action_34 (11) = happyGoto action_22
action_34 (12) = happyGoto action_23
action_34 (13) = happyGoto action_24
action_34 (14) = happyGoto action_48
action_34 (15) = happyGoto action_32
action_34 (16) = happyGoto action_33
action_34 _ = happyFail

action_35 (28) = happyShift action_25
action_35 (31) = happyShift action_34
action_35 (32) = happyShift action_35
action_35 (51) = happyShift action_36
action_35 (52) = happyShift action_37
action_35 (54) = happyShift action_27
action_35 (55) = happyShift action_28
action_35 (10) = happyGoto action_46
action_35 (11) = happyGoto action_22
action_35 (12) = happyGoto action_23
action_35 (13) = happyGoto action_24
action_35 (14) = happyGoto action_47
action_35 (15) = happyGoto action_32
action_35 (16) = happyGoto action_33
action_35 _ = happyFail

action_36 _ = happyReduce_41

action_37 _ = happyReduce_42

action_38 (18) = happyShift action_44
action_38 (40) = happyShift action_45
action_38 _ = happyFail

action_39 _ = happyReduce_9

action_40 _ = happyReduce_1

action_41 _ = happyReduce_6

action_42 (28) = happyShift action_25
action_42 (32) = happyShift action_26
action_42 (54) = happyShift action_27
action_42 (55) = happyShift action_28
action_42 (10) = happyGoto action_43
action_42 (11) = happyGoto action_22
action_42 (12) = happyGoto action_23
action_42 (13) = happyGoto action_24
action_42 _ = happyFail

action_43 (27) = happyShift action_57
action_43 (28) = happyShift action_58
action_43 (37) = happyShift action_89
action_43 _ = happyFail

action_44 (28) = happyShift action_25
action_44 (31) = happyShift action_34
action_44 (32) = happyShift action_35
action_44 (51) = happyShift action_36
action_44 (52) = happyShift action_37
action_44 (54) = happyShift action_27
action_44 (55) = happyShift action_28
action_44 (10) = happyGoto action_30
action_44 (11) = happyGoto action_22
action_44 (12) = happyGoto action_23
action_44 (13) = happyGoto action_24
action_44 (15) = happyGoto action_88
action_44 (16) = happyGoto action_33
action_44 _ = happyFail

action_45 (39) = happyShift action_12
action_45 (43) = happyShift action_13
action_45 (46) = happyShift action_14
action_45 (47) = happyShift action_15
action_45 (48) = happyShift action_16
action_45 (55) = happyShift action_17
action_45 (8) = happyGoto action_87
action_45 (9) = happyGoto action_11
action_45 _ = happyFail

action_46 (21) = happyShift action_51
action_46 (22) = happyShift action_52
action_46 (23) = happyShift action_53
action_46 (24) = happyShift action_54
action_46 (25) = happyShift action_55
action_46 (26) = happyShift action_56
action_46 (27) = happyShift action_57
action_46 (28) = happyShift action_58
action_46 (33) = happyShift action_74
action_46 _ = happyFail

action_47 (18) = happyShift action_44
action_47 (33) = happyShift action_86
action_47 _ = happyFail

action_48 (18) = happyShift action_44
action_48 _ = happyReduce_40

action_49 (28) = happyShift action_25
action_49 (31) = happyShift action_34
action_49 (32) = happyShift action_35
action_49 (51) = happyShift action_36
action_49 (52) = happyShift action_37
action_49 (54) = happyShift action_27
action_49 (55) = happyShift action_28
action_49 (10) = happyGoto action_30
action_49 (11) = happyGoto action_22
action_49 (12) = happyGoto action_23
action_49 (13) = happyGoto action_24
action_49 (16) = happyGoto action_85
action_49 _ = happyFail

action_50 (39) = happyShift action_12
action_50 (43) = happyShift action_13
action_50 (46) = happyShift action_14
action_50 (47) = happyShift action_15
action_50 (48) = happyShift action_16
action_50 (55) = happyShift action_17
action_50 (8) = happyGoto action_84
action_50 (9) = happyGoto action_11
action_50 _ = happyFail

action_51 (28) = happyShift action_25
action_51 (32) = happyShift action_26
action_51 (54) = happyShift action_27
action_51 (55) = happyShift action_28
action_51 (10) = happyGoto action_83
action_51 (11) = happyGoto action_22
action_51 (12) = happyGoto action_23
action_51 (13) = happyGoto action_24
action_51 _ = happyFail

action_52 (28) = happyShift action_25
action_52 (32) = happyShift action_26
action_52 (54) = happyShift action_27
action_52 (55) = happyShift action_28
action_52 (10) = happyGoto action_82
action_52 (11) = happyGoto action_22
action_52 (12) = happyGoto action_23
action_52 (13) = happyGoto action_24
action_52 _ = happyFail

action_53 (28) = happyShift action_25
action_53 (32) = happyShift action_26
action_53 (54) = happyShift action_27
action_53 (55) = happyShift action_28
action_53 (10) = happyGoto action_81
action_53 (11) = happyGoto action_22
action_53 (12) = happyGoto action_23
action_53 (13) = happyGoto action_24
action_53 _ = happyFail

action_54 (28) = happyShift action_25
action_54 (32) = happyShift action_26
action_54 (54) = happyShift action_27
action_54 (55) = happyShift action_28
action_54 (10) = happyGoto action_80
action_54 (11) = happyGoto action_22
action_54 (12) = happyGoto action_23
action_54 (13) = happyGoto action_24
action_54 _ = happyFail

action_55 (28) = happyShift action_25
action_55 (32) = happyShift action_26
action_55 (54) = happyShift action_27
action_55 (55) = happyShift action_28
action_55 (10) = happyGoto action_79
action_55 (11) = happyGoto action_22
action_55 (12) = happyGoto action_23
action_55 (13) = happyGoto action_24
action_55 _ = happyFail

action_56 (28) = happyShift action_25
action_56 (32) = happyShift action_26
action_56 (54) = happyShift action_27
action_56 (55) = happyShift action_28
action_56 (10) = happyGoto action_78
action_56 (11) = happyGoto action_22
action_56 (12) = happyGoto action_23
action_56 (13) = happyGoto action_24
action_56 _ = happyFail

action_57 (28) = happyShift action_25
action_57 (32) = happyShift action_26
action_57 (54) = happyShift action_27
action_57 (55) = happyShift action_28
action_57 (11) = happyGoto action_77
action_57 (12) = happyGoto action_23
action_57 (13) = happyGoto action_24
action_57 _ = happyFail

action_58 (28) = happyShift action_25
action_58 (32) = happyShift action_26
action_58 (54) = happyShift action_27
action_58 (55) = happyShift action_28
action_58 (11) = happyGoto action_76
action_58 (12) = happyGoto action_23
action_58 (13) = happyGoto action_24
action_58 _ = happyFail

action_59 (28) = happyShift action_25
action_59 (32) = happyShift action_26
action_59 (54) = happyShift action_27
action_59 (55) = happyShift action_28
action_59 (10) = happyGoto action_75
action_59 (11) = happyGoto action_22
action_59 (12) = happyGoto action_23
action_59 (13) = happyGoto action_24
action_59 _ = happyFail

action_60 (27) = happyShift action_57
action_60 (28) = happyShift action_58
action_60 (33) = happyShift action_74
action_60 _ = happyFail

action_61 _ = happyReduce_24

action_62 (28) = happyShift action_25
action_62 (32) = happyShift action_26
action_62 (54) = happyShift action_27
action_62 (55) = happyShift action_28
action_62 (12) = happyGoto action_73
action_62 (13) = happyGoto action_24
action_62 _ = happyFail

action_63 (28) = happyShift action_25
action_63 (32) = happyShift action_26
action_63 (54) = happyShift action_27
action_63 (55) = happyShift action_28
action_63 (12) = happyGoto action_72
action_63 (13) = happyGoto action_24
action_63 _ = happyFail

action_64 _ = happyReduce_15

action_65 _ = happyReduce_13

action_66 (28) = happyShift action_25
action_66 (32) = happyShift action_26
action_66 (54) = happyShift action_27
action_66 (55) = happyShift action_28
action_66 (10) = happyGoto action_71
action_66 (11) = happyGoto action_22
action_66 (12) = happyGoto action_23
action_66 (13) = happyGoto action_24
action_66 _ = happyFail

action_67 (27) = happyShift action_57
action_67 (28) = happyShift action_58
action_67 (37) = happyShift action_70
action_67 _ = happyFail

action_68 (20) = happyShift action_69
action_68 (27) = happyShift action_57
action_68 (28) = happyShift action_58
action_68 _ = happyFail

action_69 _ = happyReduce_10

action_70 (19) = happyShift action_95
action_70 _ = happyFail

action_71 (27) = happyShift action_57
action_71 (28) = happyShift action_58
action_71 (37) = happyShift action_94
action_71 _ = happyFail

action_72 _ = happyReduce_23

action_73 _ = happyReduce_22

action_74 _ = happyReduce_29

action_75 (27) = happyShift action_57
action_75 (28) = happyShift action_58
action_75 (37) = happyShift action_93
action_75 _ = happyFail

action_76 (29) = happyShift action_62
action_76 (30) = happyShift action_63
action_76 _ = happyReduce_20

action_77 (29) = happyShift action_62
action_77 (30) = happyShift action_63
action_77 _ = happyReduce_19

action_78 (27) = happyShift action_57
action_78 (28) = happyShift action_58
action_78 _ = happyReduce_39

action_79 (27) = happyShift action_57
action_79 (28) = happyShift action_58
action_79 _ = happyReduce_38

action_80 (27) = happyShift action_57
action_80 (28) = happyShift action_58
action_80 _ = happyReduce_37

action_81 (27) = happyShift action_57
action_81 (28) = happyShift action_58
action_81 _ = happyReduce_35

action_82 (27) = happyShift action_57
action_82 (28) = happyShift action_58
action_82 _ = happyReduce_36

action_83 (27) = happyShift action_57
action_83 (28) = happyShift action_58
action_83 _ = happyReduce_34

action_84 (45) = happyShift action_92
action_84 _ = happyFail

action_85 _ = happyReduce_33

action_86 _ = happyReduce_43

action_87 (41) = happyShift action_91
action_87 _ = happyFail

action_88 (17) = happyShift action_49
action_88 _ = happyReduce_31

action_89 (20) = happyShift action_90
action_89 _ = happyFail

action_90 _ = happyReduce_7

action_91 (39) = happyShift action_12
action_91 (43) = happyShift action_13
action_91 (46) = happyShift action_14
action_91 (47) = happyShift action_15
action_91 (48) = happyShift action_16
action_91 (55) = happyShift action_17
action_91 (8) = happyGoto action_98
action_91 (9) = happyGoto action_11
action_91 _ = happyFail

action_92 _ = happyReduce_17

action_93 _ = happyReduce_27

action_94 (20) = happyShift action_97
action_94 _ = happyFail

action_95 (28) = happyShift action_25
action_95 (32) = happyShift action_26
action_95 (54) = happyShift action_27
action_95 (55) = happyShift action_28
action_95 (10) = happyGoto action_96
action_95 (11) = happyGoto action_22
action_95 (12) = happyGoto action_23
action_95 (13) = happyGoto action_24
action_95 _ = happyFail

action_96 (20) = happyShift action_100
action_96 (27) = happyShift action_57
action_96 (28) = happyShift action_58
action_96 _ = happyFail

action_97 _ = happyReduce_14

action_98 (42) = happyShift action_99
action_98 _ = happyFail

action_99 _ = happyReduce_16

action_100 _ = happyReduce_11

happyReduce_1 = happyReduce 4 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Program happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 (EmptyDeclBody
	)

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (DeclBody happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (SingleDecl happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  6 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (DeclList happy_var_1 happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 _
	(HappyTerminal (IDENTIFIER happy_var_2))
	_
	 =  HappyAbsSyn7
		 (Decl happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happyReduce 6 7 happyReduction_7
happyReduction_7 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (IDENTIFIER happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (DeclArray happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (StmtList happy_var_1 NoStmt
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  8 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (StmtList happy_var_1 happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 4 9 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (IDENTIFIER happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (StmtAssign happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 7 9 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (IDENTIFIER happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (StmtAssignArray happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_2  9 happyReduction_12
happyReduction_12 _
	_
	 =  HappyAbsSyn9
		 (StmtSkip
	)

happyReduce_13 = happySpecReduce_3  9 happyReduction_13
happyReduction_13 _
	(HappyTerminal (IDENTIFIER happy_var_2))
	_
	 =  HappyAbsSyn9
		 (StmtRead happy_var_2
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 6 9 happyReduction_14
happyReduction_14 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (IDENTIFIER happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (StmtReadArray happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_3  9 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (StmtWrite happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 7 9 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (StmtIf happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 5 9 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (StmtWhile happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_1  10 happyReduction_18
happyReduction_18 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (Aexpr1 happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  10 happyReduction_19
happyReduction_19 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  10 happyReduction_20
happyReduction_20 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  11 happyReduction_21
happyReduction_21 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (Aexpr2 happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  11 happyReduction_22
happyReduction_22 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  11 happyReduction_23
happyReduction_23 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (Div happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_2  12 happyReduction_24
happyReduction_24 (HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (Neg happy_var_2
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  12 happyReduction_25
happyReduction_25 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (Aexpr3 happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  13 happyReduction_26
happyReduction_26 (HappyTerminal (IDENTIFIER happy_var_1))
	 =  HappyAbsSyn13
		 (Identifier happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happyReduce 4 13 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (IDENTIFIER happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (IdentifierArray happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_1  13 happyReduction_28
happyReduction_28 (HappyTerminal (INTLITERAL happy_var_1))
	 =  HappyAbsSyn13
		 (IntegerLiteral happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  13 happyReduction_29
happyReduction_29 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (ABrack happy_var_2
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  14 happyReduction_30
happyReduction_30 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (Bexpr1 happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  14 happyReduction_31
happyReduction_31 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (Or happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  15 happyReduction_32
happyReduction_32 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (Bexpr2 happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  15 happyReduction_33
happyReduction_33 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (And happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  16 happyReduction_34
happyReduction_34 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn16
		 (GreatThan happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  16 happyReduction_35
happyReduction_35 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn16
		 (LessThan happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  16 happyReduction_36
happyReduction_36 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn16
		 (GreatEqual happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  16 happyReduction_37
happyReduction_37 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn16
		 (LessEqual happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  16 happyReduction_38
happyReduction_38 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn16
		 (Equal happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  16 happyReduction_39
happyReduction_39 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn16
		 (NotEqual happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_2  16 happyReduction_40
happyReduction_40 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (Not happy_var_2
	)
happyReduction_40 _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  16 happyReduction_41
happyReduction_41 _
	 =  HappyAbsSyn16
		 (Boolean True
	)

happyReduce_42 = happySpecReduce_1  16 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn16
		 (Boolean False
	)

happyReduce_43 = happySpecReduce_3  16 happyReduction_43
happyReduction_43 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (BBrack happy_var_2
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 56 56 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	AND -> cont 17;
	OR -> cont 18;
	ASSIGN -> cont 19;
	SEMI -> cont 20;
	GTHAN -> cont 21;
	GEQUAL -> cont 22;
	LTHAN -> cont 23;
	LEQUAL -> cont 24;
	EQUAL -> cont 25;
	NEQUAL -> cont 26;
	PLUS -> cont 27;
	MINUS -> cont 28;
	MUL -> cont 29;
	DIV -> cont 30;
	NOT -> cont 31;
	LPAREN -> cont 32;
	RPAREN -> cont 33;
	LBRACE -> cont 34;
	RBRACE -> cont 35;
	LBRACKET -> cont 36;
	RBRACKET -> cont 37;
	COLON -> cont 38;
	IF -> cont 39;
	THEN -> cont 40;
	ELSE -> cont 41;
	FI -> cont 42;
	WHILE -> cont 43;
	DO -> cont 44;
	OD -> cont 45;
	SKIP -> cont 46;
	WRITE -> cont 47;
	READ -> cont 48;
	PROGRAM -> cont 49;
	END -> cont 50;
	TRUE -> cont 51;
	FALSE -> cont 52;
	INT -> cont 53;
	INTLITERAL happy_dollar_dollar -> cont 54;
	IDENTIFIER happy_dollar_dollar -> cont 55;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

testpar tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

data Program
	= Program DeclBody StmtList
	deriving(Show, Eq)

data DeclBody
	= DeclBody DeclList
	| EmptyDeclBody
	deriving(Show, Eq)

data DeclList
	= DeclList Decl DeclList
	| SingleDecl Decl
	| NoDecl
	deriving(Show, Eq)

data Decl
	= Decl Identifier
	| DeclArray Identifier Aexpr
	deriving(Show, Eq)

data StmtList
	= StmtList Stmt StmtList
	| NoStmt
	deriving(Show, Eq)

data Stmt
	= StmtAssign Identifier Aexpr
	| StmtAssignArray Identifier Aexpr Aexpr
	| StmtSkip
	| StmtIf Bexpr StmtList StmtList
	| StmtRead Identifier
	| StmtReadArray Identifier Aexpr
	| StmtWrite Aexpr
	| StmtWhile Bexpr StmtList
	deriving(Show, Eq)

data Aexpr
	= Aexpr1 Aexpr1
	| Plus Aexpr Aexpr1
	| Minus Aexpr Aexpr1
	deriving(Show, Eq)

data Aexpr1
	= Aexpr2 Aexpr2
	| Mul Aexpr1 Aexpr2
	| Div Aexpr1 Aexpr2
	deriving(Show, Eq)

data Aexpr2
	= Neg Aexpr3
	| Aexpr3 Aexpr3
	deriving(Show, Eq)

data Aexpr3
	= Identifier Identifier
	| IdentifierArray Identifier Aexpr
	| IntegerLiteral IntegerLiteral
	| ABrack Aexpr
	deriving(Show, Eq)

data Bexpr 
	= Bexpr1 Bexpr1
	| Or Bexpr Bexpr1
	deriving(Show, Eq)

data Bexpr1
	= Bexpr2 Bexpr2
	| And Bexpr1 Bexpr2
	deriving(Show, Eq)

data Bexpr2
	= GreatThan Aexpr Aexpr
	| LessThan Aexpr Aexpr
	| GreatEqual Aexpr Aexpr
	| LessEqual Aexpr Aexpr
	| Equal Aexpr Aexpr
	| NotEqual Aexpr Aexpr
	| Not Bexpr
	| Boolean Boolean
	| BBrack Bexpr
	deriving(Show, Eq)

type Identifier	= String
type IntegerLiteral = Int
type Boolean = Bool


getTree :: String -> Program
getTree s = testpar (alexScanTokens s)

getStmtList :: Program -> StmtList
getStmtList (Program decl stmt) = stmt


getStmt (StmtList stmt stmtlist) = (stmt, stmtlist)

try :: String -> IO ()
try s = do
	let parseTree = testpar (alexScanTokens s)
	putStrLn ("parseTree: " ++ show(parseTree))
	print "done"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

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

{-# LINE 148 "templates/GenericTemplate.hs" #-}

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
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
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
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

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

{-# LINE 311 "templates/GenericTemplate.hs" #-}
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
