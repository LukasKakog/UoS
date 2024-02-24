{-# OPTIONS_GHC -w #-}
module Grammar where
import Tokens
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18
	= HappyTerminal (TokenType)
	| HappyErrorToken Prelude.Int
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

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,420) ([0,4128,65436,7199,0,0,1,0,0,0,0,128,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,284,0,0,0,0,0,0,0,2,0,384,0,0,0,16,8192,0,0,0,2048,0,0,0,512,0,0,0,16384,0,0,0,32,0,0,0,8,0,0,0,2,0,0,32768,0,0,0,8192,0,0,0,2048,0,0,0,512,0,0,0,128,0,0,0,32,0,0,0,8,0,0,0,256,0,0,0,64,0,0,0,16,0,0,8192,0,0,0,0,0,0,0,128,64512,28799,0,0,0,6144,0,0,0,512,0,0,0,128,32768,0,32764,112,8192,0,8191,28,2048,49152,2047,7,512,61440,49663,1,128,64512,28799,0,32,65280,7199,0,8,65472,1799,0,2,65520,449,32768,0,32764,112,0,0,0,24,0,4,0,0,512,0,32768,1,128,0,28160,0,32,65280,7199,0,2048,0,0,49152,21,0,0,0,0,0,0,8192,0,0,24,2048,0,0,6,512,0,32768,1,128,0,24576,0,4128,65432,7199,0,16384,0,0,0,49410,65529,449,0,0,0,0,6144,0,0,0,0,0,0,0,0,0,0,0,96,0,0,0,0,0,0,0,32,0,0,0,4,0,0,0,1,0,0,0,0,0,0,50944,1,0,0,512,0,45056,1,128,0,0,0,0,0,0,0,0,0,0,49152,21,0,0,0,128,0,0,0,0,0,16,0,2,0,0,1024,0,0,0,256,0,0,0,64,0,0,0,16,0,0,0,4,0,0,0,32,0,0,0,8,0,0,4096,0,0,0,32768,0,0,0,32768,0,0,0,2048,0,0,0,2048,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,6144,0,0,0,0,0,2,65520,449,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1536,0,512,0,0,0,32,0,0,0,0,0,0,0,16,0,0,512,0,47104,1,256,0,0,0,1884,0,0,0,8,0,1536,0,2,0,384,0,32768,0,0,0,256,0,0,0,4,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,4096,0,0,49152,17,0,0,28672,4,0,0,0,0,24576,0,4096,0,0,0,0,2080,0,0,0,0,16384,0,2048,0,0,0,16,0,0,0,0,0,384,0,1,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,128,0,0,0,0,0,0,512,63745,49663,1,128,0,0,0,0,0,0,0,8,0,0,0,2,0,0,32768,16448,32766,112,0,32,0,0,0,0,0,0,0,8,0,0,128,0,27648,0,32,0,6912,0,8,0,1536,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,6,1024,0,0,0,0,0,0,0,348,0,0,0,16,0,0,0,4,0,0,0,0,1,0,0,0,0,0,0,1040,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,512,0,0,0,0,0,384,0,2048,2,0,8192,36880,8191,28,2048,0,0,0,512,0,47104,1,0,2,0,0,32768,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,512,0,0,0,1,0,0,4128,65424,7199,0,8192,0,0,0,0,4,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseCalc","Start","End","Loop","HashList","Ent","Func","BoolTiles","IfStatement","List","Row","Block","Operations","Boolean","Math","Prim","'-'","'*'","'/'","'('","')'","'='","'+'","'<'","\"==\"","','","'['","']'","'{'","'}'","';'","'#'","import","print","if","then","else","for","create","blank","flipx","flipy","numCol","numRow","rotate","subtile","conj","neg","supersize","stack","join","\"&&\"","\"||\"","not","true","false","tokGr","var","int","%eof"]
        bit_start = st Prelude.* 62
        bit_end = (st Prelude.+ 1) Prelude.* 62
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..61]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (22) = happyShift action_10
action_0 (29) = happyShift action_11
action_0 (35) = happyShift action_2
action_0 (36) = happyShift action_12
action_0 (37) = happyShift action_13
action_0 (40) = happyShift action_14
action_0 (41) = happyShift action_15
action_0 (42) = happyShift action_16
action_0 (43) = happyShift action_17
action_0 (44) = happyShift action_18
action_0 (45) = happyShift action_19
action_0 (46) = happyShift action_20
action_0 (47) = happyShift action_21
action_0 (48) = happyShift action_22
action_0 (49) = happyShift action_23
action_0 (50) = happyShift action_24
action_0 (51) = happyShift action_25
action_0 (52) = happyShift action_26
action_0 (53) = happyShift action_27
action_0 (59) = happyShift action_28
action_0 (60) = happyShift action_29
action_0 (61) = happyShift action_30
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 (9) = happyGoto action_6
action_0 (10) = happyGoto action_7
action_0 (17) = happyGoto action_8
action_0 (18) = happyGoto action_9
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (35) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (60) = happyShift action_57
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (62) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_2

action_5 (33) = happyShift action_56
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_13

action_7 _ = happyReduce_26

action_8 (19) = happyShift action_52
action_8 (20) = happyShift action_53
action_8 (21) = happyShift action_54
action_8 (25) = happyShift action_55
action_8 _ = happyReduce_29

action_9 _ = happyReduce_51

action_10 (22) = happyShift action_10
action_10 (60) = happyShift action_51
action_10 (61) = happyShift action_30
action_10 (17) = happyGoto action_50
action_10 (18) = happyGoto action_9
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (59) = happyShift action_49
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (22) = happyShift action_48
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (22) = happyShift action_47
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (22) = happyShift action_46
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (29) = happyShift action_45
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (22) = happyShift action_44
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (22) = happyShift action_43
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (22) = happyShift action_42
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (22) = happyShift action_41
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (22) = happyShift action_40
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (22) = happyShift action_39
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (22) = happyShift action_38
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (22) = happyShift action_37
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (22) = happyShift action_36
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (22) = happyShift action_35
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (29) = happyShift action_34
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (29) = happyShift action_33
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (29) = happyShift action_32
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (24) = happyShift action_31
action_29 _ = happyReduce_53

action_30 _ = happyReduce_52

action_31 (22) = happyShift action_10
action_31 (41) = happyShift action_15
action_31 (42) = happyShift action_16
action_31 (43) = happyShift action_17
action_31 (44) = happyShift action_18
action_31 (45) = happyShift action_19
action_31 (46) = happyShift action_20
action_31 (47) = happyShift action_21
action_31 (48) = happyShift action_22
action_31 (49) = happyShift action_23
action_31 (50) = happyShift action_24
action_31 (51) = happyShift action_25
action_31 (52) = happyShift action_26
action_31 (53) = happyShift action_27
action_31 (59) = happyShift action_28
action_31 (60) = happyShift action_51
action_31 (61) = happyShift action_30
action_31 (9) = happyGoto action_91
action_31 (10) = happyGoto action_7
action_31 (17) = happyGoto action_8
action_31 (18) = happyGoto action_9
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (60) = happyShift action_51
action_32 (61) = happyShift action_30
action_32 (18) = happyGoto action_90
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (60) = happyShift action_88
action_33 (12) = happyGoto action_89
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (60) = happyShift action_88
action_34 (12) = happyGoto action_87
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (22) = happyShift action_10
action_35 (41) = happyShift action_15
action_35 (42) = happyShift action_16
action_35 (43) = happyShift action_17
action_35 (44) = happyShift action_18
action_35 (45) = happyShift action_19
action_35 (46) = happyShift action_20
action_35 (47) = happyShift action_21
action_35 (48) = happyShift action_22
action_35 (49) = happyShift action_23
action_35 (50) = happyShift action_24
action_35 (51) = happyShift action_25
action_35 (52) = happyShift action_26
action_35 (53) = happyShift action_27
action_35 (59) = happyShift action_28
action_35 (60) = happyShift action_51
action_35 (61) = happyShift action_30
action_35 (9) = happyGoto action_86
action_35 (10) = happyGoto action_7
action_35 (17) = happyGoto action_8
action_35 (18) = happyGoto action_9
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (22) = happyShift action_10
action_36 (41) = happyShift action_15
action_36 (42) = happyShift action_16
action_36 (43) = happyShift action_17
action_36 (44) = happyShift action_18
action_36 (45) = happyShift action_19
action_36 (46) = happyShift action_20
action_36 (47) = happyShift action_21
action_36 (48) = happyShift action_22
action_36 (49) = happyShift action_23
action_36 (50) = happyShift action_24
action_36 (51) = happyShift action_25
action_36 (52) = happyShift action_26
action_36 (53) = happyShift action_27
action_36 (59) = happyShift action_28
action_36 (60) = happyShift action_51
action_36 (61) = happyShift action_30
action_36 (9) = happyGoto action_85
action_36 (10) = happyGoto action_7
action_36 (17) = happyGoto action_8
action_36 (18) = happyGoto action_9
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (22) = happyShift action_10
action_37 (41) = happyShift action_15
action_37 (42) = happyShift action_16
action_37 (43) = happyShift action_17
action_37 (44) = happyShift action_18
action_37 (45) = happyShift action_19
action_37 (46) = happyShift action_20
action_37 (47) = happyShift action_21
action_37 (48) = happyShift action_22
action_37 (49) = happyShift action_23
action_37 (50) = happyShift action_24
action_37 (51) = happyShift action_25
action_37 (52) = happyShift action_26
action_37 (53) = happyShift action_27
action_37 (59) = happyShift action_28
action_37 (60) = happyShift action_51
action_37 (61) = happyShift action_30
action_37 (9) = happyGoto action_84
action_37 (10) = happyGoto action_7
action_37 (17) = happyGoto action_8
action_37 (18) = happyGoto action_9
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (22) = happyShift action_10
action_38 (41) = happyShift action_15
action_38 (42) = happyShift action_16
action_38 (43) = happyShift action_17
action_38 (44) = happyShift action_18
action_38 (45) = happyShift action_19
action_38 (46) = happyShift action_20
action_38 (47) = happyShift action_21
action_38 (48) = happyShift action_22
action_38 (49) = happyShift action_23
action_38 (50) = happyShift action_24
action_38 (51) = happyShift action_25
action_38 (52) = happyShift action_26
action_38 (53) = happyShift action_27
action_38 (59) = happyShift action_28
action_38 (60) = happyShift action_51
action_38 (61) = happyShift action_30
action_38 (9) = happyGoto action_83
action_38 (10) = happyGoto action_7
action_38 (17) = happyGoto action_8
action_38 (18) = happyGoto action_9
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (22) = happyShift action_10
action_39 (41) = happyShift action_15
action_39 (42) = happyShift action_16
action_39 (43) = happyShift action_17
action_39 (44) = happyShift action_18
action_39 (45) = happyShift action_19
action_39 (46) = happyShift action_20
action_39 (47) = happyShift action_21
action_39 (48) = happyShift action_22
action_39 (49) = happyShift action_23
action_39 (50) = happyShift action_24
action_39 (51) = happyShift action_25
action_39 (52) = happyShift action_26
action_39 (53) = happyShift action_27
action_39 (59) = happyShift action_28
action_39 (60) = happyShift action_51
action_39 (61) = happyShift action_30
action_39 (9) = happyGoto action_82
action_39 (10) = happyGoto action_7
action_39 (17) = happyGoto action_8
action_39 (18) = happyGoto action_9
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (22) = happyShift action_10
action_40 (41) = happyShift action_15
action_40 (42) = happyShift action_16
action_40 (43) = happyShift action_17
action_40 (44) = happyShift action_18
action_40 (45) = happyShift action_19
action_40 (46) = happyShift action_20
action_40 (47) = happyShift action_21
action_40 (48) = happyShift action_22
action_40 (49) = happyShift action_23
action_40 (50) = happyShift action_24
action_40 (51) = happyShift action_25
action_40 (52) = happyShift action_26
action_40 (53) = happyShift action_27
action_40 (59) = happyShift action_28
action_40 (60) = happyShift action_51
action_40 (61) = happyShift action_30
action_40 (9) = happyGoto action_81
action_40 (10) = happyGoto action_7
action_40 (17) = happyGoto action_8
action_40 (18) = happyGoto action_9
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (22) = happyShift action_10
action_41 (41) = happyShift action_15
action_41 (42) = happyShift action_16
action_41 (43) = happyShift action_17
action_41 (44) = happyShift action_18
action_41 (45) = happyShift action_19
action_41 (46) = happyShift action_20
action_41 (47) = happyShift action_21
action_41 (48) = happyShift action_22
action_41 (49) = happyShift action_23
action_41 (50) = happyShift action_24
action_41 (51) = happyShift action_25
action_41 (52) = happyShift action_26
action_41 (53) = happyShift action_27
action_41 (59) = happyShift action_28
action_41 (60) = happyShift action_51
action_41 (61) = happyShift action_30
action_41 (9) = happyGoto action_80
action_41 (10) = happyGoto action_7
action_41 (17) = happyGoto action_8
action_41 (18) = happyGoto action_9
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (22) = happyShift action_10
action_42 (41) = happyShift action_15
action_42 (42) = happyShift action_16
action_42 (43) = happyShift action_17
action_42 (44) = happyShift action_18
action_42 (45) = happyShift action_19
action_42 (46) = happyShift action_20
action_42 (47) = happyShift action_21
action_42 (48) = happyShift action_22
action_42 (49) = happyShift action_23
action_42 (50) = happyShift action_24
action_42 (51) = happyShift action_25
action_42 (52) = happyShift action_26
action_42 (53) = happyShift action_27
action_42 (59) = happyShift action_28
action_42 (60) = happyShift action_51
action_42 (61) = happyShift action_30
action_42 (9) = happyGoto action_79
action_42 (10) = happyGoto action_7
action_42 (17) = happyGoto action_8
action_42 (18) = happyGoto action_9
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (22) = happyShift action_10
action_43 (41) = happyShift action_15
action_43 (42) = happyShift action_16
action_43 (43) = happyShift action_17
action_43 (44) = happyShift action_18
action_43 (45) = happyShift action_19
action_43 (46) = happyShift action_20
action_43 (47) = happyShift action_21
action_43 (48) = happyShift action_22
action_43 (49) = happyShift action_23
action_43 (50) = happyShift action_24
action_43 (51) = happyShift action_25
action_43 (52) = happyShift action_26
action_43 (53) = happyShift action_27
action_43 (59) = happyShift action_28
action_43 (60) = happyShift action_51
action_43 (61) = happyShift action_30
action_43 (9) = happyGoto action_78
action_43 (10) = happyGoto action_7
action_43 (17) = happyGoto action_8
action_43 (18) = happyGoto action_9
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (60) = happyShift action_51
action_44 (61) = happyShift action_30
action_44 (18) = happyGoto action_77
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (29) = happyShift action_76
action_45 (13) = happyGoto action_75
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (22) = happyShift action_10
action_46 (60) = happyShift action_51
action_46 (61) = happyShift action_30
action_46 (17) = happyGoto action_74
action_46 (18) = happyGoto action_9
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (22) = happyShift action_70
action_47 (56) = happyShift action_71
action_47 (57) = happyShift action_72
action_47 (58) = happyShift action_73
action_47 (60) = happyShift action_51
action_47 (61) = happyShift action_30
action_47 (15) = happyGoto action_67
action_47 (16) = happyGoto action_68
action_47 (17) = happyGoto action_69
action_47 (18) = happyGoto action_9
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (22) = happyShift action_10
action_48 (41) = happyShift action_15
action_48 (42) = happyShift action_16
action_48 (43) = happyShift action_17
action_48 (44) = happyShift action_18
action_48 (45) = happyShift action_19
action_48 (46) = happyShift action_20
action_48 (47) = happyShift action_21
action_48 (48) = happyShift action_22
action_48 (49) = happyShift action_23
action_48 (50) = happyShift action_24
action_48 (51) = happyShift action_25
action_48 (52) = happyShift action_26
action_48 (53) = happyShift action_27
action_48 (59) = happyShift action_28
action_48 (60) = happyShift action_51
action_48 (61) = happyShift action_30
action_48 (9) = happyGoto action_66
action_48 (10) = happyGoto action_7
action_48 (17) = happyGoto action_8
action_48 (18) = happyGoto action_9
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (30) = happyShift action_65
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (19) = happyShift action_52
action_50 (20) = happyShift action_53
action_50 (21) = happyShift action_54
action_50 (23) = happyShift action_64
action_50 (25) = happyShift action_55
action_50 _ = happyFail (happyExpListPerState 50)

action_51 _ = happyReduce_53

action_52 (22) = happyShift action_10
action_52 (60) = happyShift action_51
action_52 (61) = happyShift action_30
action_52 (17) = happyGoto action_63
action_52 (18) = happyGoto action_9
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (22) = happyShift action_10
action_53 (60) = happyShift action_51
action_53 (61) = happyShift action_30
action_53 (17) = happyGoto action_62
action_53 (18) = happyGoto action_9
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (22) = happyShift action_10
action_54 (60) = happyShift action_51
action_54 (61) = happyShift action_30
action_54 (17) = happyGoto action_61
action_54 (18) = happyGoto action_9
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (22) = happyShift action_10
action_55 (60) = happyShift action_51
action_55 (61) = happyShift action_30
action_55 (17) = happyGoto action_60
action_55 (18) = happyGoto action_9
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (22) = happyShift action_10
action_56 (29) = happyShift action_11
action_56 (36) = happyShift action_12
action_56 (37) = happyShift action_13
action_56 (40) = happyShift action_14
action_56 (41) = happyShift action_15
action_56 (42) = happyShift action_16
action_56 (43) = happyShift action_17
action_56 (44) = happyShift action_18
action_56 (45) = happyShift action_19
action_56 (46) = happyShift action_20
action_56 (47) = happyShift action_21
action_56 (48) = happyShift action_22
action_56 (49) = happyShift action_23
action_56 (50) = happyShift action_24
action_56 (51) = happyShift action_25
action_56 (52) = happyShift action_26
action_56 (53) = happyShift action_27
action_56 (59) = happyShift action_28
action_56 (60) = happyShift action_29
action_56 (61) = happyShift action_30
action_56 (5) = happyGoto action_59
action_56 (8) = happyGoto action_5
action_56 (9) = happyGoto action_6
action_56 (10) = happyGoto action_7
action_56 (17) = happyGoto action_8
action_56 (18) = happyGoto action_9
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (33) = happyShift action_58
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (22) = happyShift action_10
action_58 (29) = happyShift action_11
action_58 (35) = happyShift action_2
action_58 (36) = happyShift action_12
action_58 (37) = happyShift action_13
action_58 (40) = happyShift action_14
action_58 (41) = happyShift action_15
action_58 (42) = happyShift action_16
action_58 (43) = happyShift action_17
action_58 (44) = happyShift action_18
action_58 (45) = happyShift action_19
action_58 (46) = happyShift action_20
action_58 (47) = happyShift action_21
action_58 (48) = happyShift action_22
action_58 (49) = happyShift action_23
action_58 (50) = happyShift action_24
action_58 (51) = happyShift action_25
action_58 (52) = happyShift action_26
action_58 (53) = happyShift action_27
action_58 (59) = happyShift action_28
action_58 (60) = happyShift action_29
action_58 (61) = happyShift action_30
action_58 (4) = happyGoto action_118
action_58 (5) = happyGoto action_4
action_58 (8) = happyGoto action_5
action_58 (9) = happyGoto action_6
action_58 (10) = happyGoto action_7
action_58 (17) = happyGoto action_8
action_58 (18) = happyGoto action_9
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_3

action_60 (20) = happyShift action_53
action_60 (21) = happyShift action_54
action_60 _ = happyReduce_46

action_61 _ = happyReduce_49

action_62 _ = happyReduce_48

action_63 (20) = happyShift action_53
action_63 (21) = happyShift action_54
action_63 _ = happyReduce_47

action_64 _ = happyReduce_50

action_65 (24) = happyShift action_117
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (23) = happyShift action_116
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (23) = happyShift action_115
action_67 _ = happyFail (happyExpListPerState 67)

action_68 _ = happyReduce_41

action_69 (19) = happyShift action_52
action_69 (20) = happyShift action_53
action_69 (21) = happyShift action_54
action_69 (25) = happyShift action_55
action_69 (26) = happyShift action_113
action_69 (27) = happyShift action_114
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (22) = happyShift action_10
action_70 (57) = happyShift action_72
action_70 (58) = happyShift action_73
action_70 (60) = happyShift action_51
action_70 (61) = happyShift action_30
action_70 (16) = happyGoto action_111
action_70 (17) = happyGoto action_112
action_70 (18) = happyGoto action_9
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (22) = happyShift action_110
action_71 _ = happyFail (happyExpListPerState 71)

action_72 _ = happyReduce_44

action_73 _ = happyReduce_45

action_74 (19) = happyShift action_52
action_74 (20) = happyShift action_53
action_74 (21) = happyShift action_54
action_74 (23) = happyShift action_109
action_74 (25) = happyShift action_55
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (30) = happyShift action_108
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (61) = happyShift action_107
action_76 (14) = happyGoto action_106
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (28) = happyShift action_105
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (23) = happyShift action_104
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (23) = happyShift action_103
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (23) = happyShift action_102
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (23) = happyShift action_101
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (23) = happyShift action_100
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (28) = happyShift action_99
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (28) = happyShift action_98
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (23) = happyShift action_97
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (28) = happyShift action_96
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (30) = happyShift action_95
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (28) = happyShift action_94
action_88 _ = happyReduce_32

action_89 (30) = happyShift action_93
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (30) = happyShift action_92
action_90 _ = happyFail (happyExpListPerState 90)

action_91 _ = happyReduce_9

action_92 _ = happyReduce_25

action_93 _ = happyReduce_20

action_94 (60) = happyShift action_88
action_94 (12) = happyGoto action_133
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_21

action_96 (60) = happyShift action_51
action_96 (61) = happyShift action_30
action_96 (18) = happyGoto action_132
action_96 _ = happyFail (happyExpListPerState 96)

action_97 _ = happyReduce_28

action_98 (22) = happyShift action_10
action_98 (41) = happyShift action_15
action_98 (42) = happyShift action_16
action_98 (43) = happyShift action_17
action_98 (44) = happyShift action_18
action_98 (45) = happyShift action_19
action_98 (46) = happyShift action_20
action_98 (47) = happyShift action_21
action_98 (48) = happyShift action_22
action_98 (49) = happyShift action_23
action_98 (50) = happyShift action_24
action_98 (51) = happyShift action_25
action_98 (52) = happyShift action_26
action_98 (53) = happyShift action_27
action_98 (59) = happyShift action_28
action_98 (60) = happyShift action_51
action_98 (61) = happyShift action_30
action_98 (9) = happyGoto action_131
action_98 (10) = happyGoto action_7
action_98 (17) = happyGoto action_8
action_98 (18) = happyGoto action_9
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (22) = happyShift action_130
action_99 _ = happyFail (happyExpListPerState 99)

action_100 _ = happyReduce_22

action_101 _ = happyReduce_17

action_102 _ = happyReduce_16

action_103 _ = happyReduce_18

action_104 _ = happyReduce_19

action_105 (60) = happyShift action_51
action_105 (61) = happyShift action_30
action_105 (18) = happyGoto action_129
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (30) = happyShift action_128
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (28) = happyShift action_127
action_107 _ = happyReduce_36

action_108 _ = happyReduce_14

action_109 (31) = happyShift action_126
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (22) = happyShift action_70
action_110 (56) = happyShift action_71
action_110 (57) = happyShift action_72
action_110 (58) = happyShift action_73
action_110 (60) = happyShift action_51
action_110 (61) = happyShift action_30
action_110 (15) = happyGoto action_125
action_110 (16) = happyGoto action_68
action_110 (17) = happyGoto action_69
action_110 (18) = happyGoto action_9
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (23) = happyShift action_124
action_111 _ = happyFail (happyExpListPerState 111)

action_112 (19) = happyShift action_52
action_112 (20) = happyShift action_53
action_112 (21) = happyShift action_54
action_112 (23) = happyShift action_64
action_112 (25) = happyShift action_55
action_112 (26) = happyShift action_113
action_112 (27) = happyShift action_114
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (22) = happyShift action_10
action_113 (60) = happyShift action_51
action_113 (61) = happyShift action_30
action_113 (17) = happyGoto action_123
action_113 (18) = happyGoto action_9
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (22) = happyShift action_10
action_114 (60) = happyShift action_51
action_114 (61) = happyShift action_30
action_114 (17) = happyGoto action_122
action_114 (18) = happyGoto action_9
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (38) = happyShift action_121
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (33) = happyShift action_120
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (29) = happyShift action_119
action_117 _ = happyFail (happyExpListPerState 117)

action_118 _ = happyReduce_1

action_119 (60) = happyShift action_88
action_119 (12) = happyGoto action_148
action_119 _ = happyFail (happyExpListPerState 119)

action_120 _ = happyReduce_4

action_121 (31) = happyShift action_147
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (19) = happyShift action_52
action_122 (20) = happyShift action_53
action_122 (21) = happyShift action_54
action_122 (25) = happyShift action_55
action_122 _ = happyReduce_43

action_123 (19) = happyShift action_52
action_123 (20) = happyShift action_53
action_123 (21) = happyShift action_54
action_123 (25) = happyShift action_55
action_123 _ = happyReduce_42

action_124 (54) = happyShift action_145
action_124 (55) = happyShift action_146
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (23) = happyShift action_144
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (34) = happyShift action_142
action_126 (40) = happyShift action_143
action_126 (6) = happyGoto action_140
action_126 (7) = happyGoto action_141
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (61) = happyShift action_107
action_127 (14) = happyGoto action_139
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (28) = happyShift action_138
action_128 _ = happyReduce_34

action_129 (23) = happyShift action_137
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (60) = happyShift action_51
action_130 (61) = happyShift action_30
action_130 (18) = happyGoto action_136
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (23) = happyShift action_135
action_131 _ = happyFail (happyExpListPerState 131)

action_132 (23) = happyShift action_134
action_132 _ = happyFail (happyExpListPerState 132)

action_133 _ = happyReduce_33

action_134 _ = happyReduce_24

action_135 _ = happyReduce_27

action_136 (28) = happyShift action_157
action_136 _ = happyFail (happyExpListPerState 136)

action_137 _ = happyReduce_15

action_138 (29) = happyShift action_76
action_138 (13) = happyGoto action_156
action_138 _ = happyFail (happyExpListPerState 138)

action_139 _ = happyReduce_37

action_140 (32) = happyShift action_155
action_140 _ = happyFail (happyExpListPerState 140)

action_141 _ = happyReduce_6

action_142 (22) = happyShift action_10
action_142 (29) = happyShift action_11
action_142 (37) = happyShift action_13
action_142 (40) = happyShift action_14
action_142 (41) = happyShift action_15
action_142 (42) = happyShift action_16
action_142 (43) = happyShift action_17
action_142 (44) = happyShift action_18
action_142 (45) = happyShift action_19
action_142 (46) = happyShift action_20
action_142 (47) = happyShift action_21
action_142 (48) = happyShift action_22
action_142 (49) = happyShift action_23
action_142 (50) = happyShift action_24
action_142 (51) = happyShift action_25
action_142 (52) = happyShift action_26
action_142 (53) = happyShift action_27
action_142 (59) = happyShift action_28
action_142 (60) = happyShift action_29
action_142 (61) = happyShift action_30
action_142 (8) = happyGoto action_154
action_142 (9) = happyGoto action_6
action_142 (10) = happyGoto action_7
action_142 (17) = happyGoto action_8
action_142 (18) = happyGoto action_9
action_142 _ = happyFail (happyExpListPerState 142)

action_143 (22) = happyShift action_153
action_143 _ = happyFail (happyExpListPerState 143)

action_144 _ = happyReduce_40

action_145 (22) = happyShift action_152
action_145 _ = happyFail (happyExpListPerState 145)

action_146 (22) = happyShift action_151
action_146 _ = happyFail (happyExpListPerState 146)

action_147 (22) = happyShift action_10
action_147 (29) = happyShift action_11
action_147 (37) = happyShift action_13
action_147 (40) = happyShift action_14
action_147 (41) = happyShift action_15
action_147 (42) = happyShift action_16
action_147 (43) = happyShift action_17
action_147 (44) = happyShift action_18
action_147 (45) = happyShift action_19
action_147 (46) = happyShift action_20
action_147 (47) = happyShift action_21
action_147 (48) = happyShift action_22
action_147 (49) = happyShift action_23
action_147 (50) = happyShift action_24
action_147 (51) = happyShift action_25
action_147 (52) = happyShift action_26
action_147 (53) = happyShift action_27
action_147 (59) = happyShift action_28
action_147 (60) = happyShift action_29
action_147 (61) = happyShift action_30
action_147 (8) = happyGoto action_150
action_147 (9) = happyGoto action_6
action_147 (10) = happyGoto action_7
action_147 (17) = happyGoto action_8
action_147 (18) = happyGoto action_9
action_147 _ = happyFail (happyExpListPerState 147)

action_148 (30) = happyShift action_149
action_148 _ = happyFail (happyExpListPerState 148)

action_149 _ = happyReduce_10

action_150 (32) = happyShift action_163
action_150 _ = happyFail (happyExpListPerState 150)

action_151 (22) = happyShift action_10
action_151 (57) = happyShift action_72
action_151 (58) = happyShift action_73
action_151 (60) = happyShift action_51
action_151 (61) = happyShift action_30
action_151 (16) = happyGoto action_162
action_151 (17) = happyGoto action_69
action_151 (18) = happyGoto action_9
action_151 _ = happyFail (happyExpListPerState 151)

action_152 (22) = happyShift action_10
action_152 (57) = happyShift action_72
action_152 (58) = happyShift action_73
action_152 (60) = happyShift action_51
action_152 (61) = happyShift action_30
action_152 (16) = happyGoto action_161
action_152 (17) = happyGoto action_69
action_152 (18) = happyGoto action_9
action_152 _ = happyFail (happyExpListPerState 152)

action_153 (22) = happyShift action_10
action_153 (60) = happyShift action_51
action_153 (61) = happyShift action_30
action_153 (17) = happyGoto action_160
action_153 (18) = happyGoto action_9
action_153 _ = happyFail (happyExpListPerState 153)

action_154 (34) = happyShift action_142
action_154 (7) = happyGoto action_159
action_154 _ = happyReduce_8

action_155 _ = happyReduce_11

action_156 _ = happyReduce_35

action_157 (60) = happyShift action_51
action_157 (61) = happyShift action_30
action_157 (18) = happyGoto action_158
action_157 _ = happyFail (happyExpListPerState 157)

action_158 (23) = happyShift action_169
action_158 _ = happyFail (happyExpListPerState 158)

action_159 _ = happyReduce_7

action_160 (19) = happyShift action_52
action_160 (20) = happyShift action_53
action_160 (21) = happyShift action_54
action_160 (23) = happyShift action_168
action_160 (25) = happyShift action_55
action_160 _ = happyFail (happyExpListPerState 160)

action_161 (23) = happyShift action_167
action_161 _ = happyFail (happyExpListPerState 161)

action_162 (23) = happyShift action_166
action_162 _ = happyFail (happyExpListPerState 162)

action_163 (39) = happyShift action_165
action_163 (11) = happyGoto action_164
action_163 _ = happyFail (happyExpListPerState 163)

action_164 _ = happyReduce_12

action_165 (31) = happyShift action_172
action_165 (37) = happyShift action_173
action_165 _ = happyFail (happyExpListPerState 165)

action_166 _ = happyReduce_39

action_167 _ = happyReduce_38

action_168 (31) = happyShift action_171
action_168 _ = happyFail (happyExpListPerState 168)

action_169 (28) = happyShift action_170
action_169 _ = happyFail (happyExpListPerState 169)

action_170 (60) = happyShift action_51
action_170 (61) = happyShift action_30
action_170 (18) = happyGoto action_177
action_170 _ = happyFail (happyExpListPerState 170)

action_171 (34) = happyShift action_142
action_171 (40) = happyShift action_143
action_171 (6) = happyGoto action_176
action_171 (7) = happyGoto action_141
action_171 _ = happyFail (happyExpListPerState 171)

action_172 (22) = happyShift action_10
action_172 (29) = happyShift action_11
action_172 (37) = happyShift action_13
action_172 (40) = happyShift action_14
action_172 (41) = happyShift action_15
action_172 (42) = happyShift action_16
action_172 (43) = happyShift action_17
action_172 (44) = happyShift action_18
action_172 (45) = happyShift action_19
action_172 (46) = happyShift action_20
action_172 (47) = happyShift action_21
action_172 (48) = happyShift action_22
action_172 (49) = happyShift action_23
action_172 (50) = happyShift action_24
action_172 (51) = happyShift action_25
action_172 (52) = happyShift action_26
action_172 (53) = happyShift action_27
action_172 (59) = happyShift action_28
action_172 (60) = happyShift action_29
action_172 (61) = happyShift action_30
action_172 (8) = happyGoto action_175
action_172 (9) = happyGoto action_6
action_172 (10) = happyGoto action_7
action_172 (17) = happyGoto action_8
action_172 (18) = happyGoto action_9
action_172 _ = happyFail (happyExpListPerState 172)

action_173 (22) = happyShift action_174
action_173 _ = happyFail (happyExpListPerState 173)

action_174 (22) = happyShift action_70
action_174 (56) = happyShift action_71
action_174 (57) = happyShift action_72
action_174 (58) = happyShift action_73
action_174 (60) = happyShift action_51
action_174 (61) = happyShift action_30
action_174 (15) = happyGoto action_181
action_174 (16) = happyGoto action_68
action_174 (17) = happyGoto action_69
action_174 (18) = happyGoto action_9
action_174 _ = happyFail (happyExpListPerState 174)

action_175 (32) = happyShift action_180
action_175 _ = happyFail (happyExpListPerState 175)

action_176 (32) = happyShift action_179
action_176 _ = happyFail (happyExpListPerState 176)

action_177 (23) = happyShift action_178
action_177 _ = happyFail (happyExpListPerState 177)

action_178 _ = happyReduce_23

action_179 _ = happyReduce_5

action_180 _ = happyReduce_31

action_181 (23) = happyShift action_182
action_181 _ = happyFail (happyExpListPerState 181)

action_182 (38) = happyShift action_183
action_182 _ = happyFail (happyExpListPerState 182)

action_183 (31) = happyShift action_184
action_183 _ = happyFail (happyExpListPerState 183)

action_184 (22) = happyShift action_10
action_184 (29) = happyShift action_11
action_184 (37) = happyShift action_13
action_184 (40) = happyShift action_14
action_184 (41) = happyShift action_15
action_184 (42) = happyShift action_16
action_184 (43) = happyShift action_17
action_184 (44) = happyShift action_18
action_184 (45) = happyShift action_19
action_184 (46) = happyShift action_20
action_184 (47) = happyShift action_21
action_184 (48) = happyShift action_22
action_184 (49) = happyShift action_23
action_184 (50) = happyShift action_24
action_184 (51) = happyShift action_25
action_184 (52) = happyShift action_26
action_184 (53) = happyShift action_27
action_184 (59) = happyShift action_28
action_184 (60) = happyShift action_29
action_184 (61) = happyShift action_30
action_184 (8) = happyGoto action_185
action_184 (9) = happyGoto action_6
action_184 (10) = happyGoto action_7
action_184 (17) = happyGoto action_8
action_184 (18) = happyGoto action_9
action_184 _ = happyFail (happyExpListPerState 184)

action_185 (32) = happyShift action_186
action_185 _ = happyFail (happyExpListPerState 185)

action_186 (39) = happyShift action_165
action_186 (11) = happyGoto action_187
action_186 _ = happyFail (happyExpListPerState 186)

action_187 _ = happyReduce_30

happyReduce_1 = happyReduce 4 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar _ happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Imports happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn5
		 (Semis happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 5 5 happyReduction_4
happyReduction_4 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Print happy_var_3
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 7 6 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (For happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_3)
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Hashes happy_var_2 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  7 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Hash happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  8 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_3)
	_
	(HappyTerminal (TokenVar _ happy_var_1))
	 =  HappyAbsSyn8
		 (Assign happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 7 8 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenGroup _ happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TGroup happy_var_2 happy_var_6
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 7 8 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (For happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 9 8 happyReduction_12
happyReduction_12 ((HappyAbsSyn11  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (If happy_var_3 happy_var_7 happy_var_9
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_1  8 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happyReduce 4 9 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Tile happy_var_3
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 6 9 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Blank happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 4 9 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (NumCol happy_var_3
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 4 9 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (NumRow happy_var_3
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 4 9 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (FlipY happy_var_3
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 4 9 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (FlipX happy_var_3
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 4 9 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Join happy_var_3
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 4 9 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Stack happy_var_3
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 4 9 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Rotate happy_var_3
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 12 9 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_11) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Sub happy_var_3 happy_var_6 happy_var_8 happy_var_11
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 6 9 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Super happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 4 9 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenGroup _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Accessor happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_1  9 happyReduction_26
happyReduction_26 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happyReduce 6 10 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (Conjunction happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 4 10 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (Negation happy_var_3
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_1  10 happyReduction_29
happyReduction_29 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happyReduce 10 11 happyReduction_30
happyReduction_30 ((HappyAbsSyn11  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (ElseIf happy_var_4 happy_var_8 happy_var_10
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 4 11 happyReduction_31
happyReduction_31 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (Else happy_var_3
	) `HappyStk` happyRest

happyReduce_32 = happySpecReduce_1  12 happyReduction_32
happyReduction_32 (HappyTerminal (TokenVar _ happy_var_1))
	 =  HappyAbsSyn12
		 (Single happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  12 happyReduction_33
happyReduction_33 (HappyAbsSyn12  happy_var_3)
	_
	(HappyTerminal (TokenVar _ happy_var_1))
	 =  HappyAbsSyn12
		 (List happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  13 happyReduction_34
happyReduction_34 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Row happy_var_2
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happyReduce 5 13 happyReduction_35
happyReduction_35 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Rows happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_36 = happySpecReduce_1  14 happyReduction_36
happyReduction_36 (HappyTerminal (TokenDigit _ happy_var_1))
	 =  HappyAbsSyn14
		 (Block happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  14 happyReduction_37
happyReduction_37 (HappyAbsSyn14  happy_var_3)
	_
	(HappyTerminal (TokenDigit _ happy_var_1))
	 =  HappyAbsSyn14
		 (Blocks happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happyReduce 7 15 happyReduction_38
happyReduction_38 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (And happy_var_2 happy_var_6
	) `HappyStk` happyRest

happyReduce_39 = happyReduce 7 15 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Or happy_var_2 happy_var_6
	) `HappyStk` happyRest

happyReduce_40 = happyReduce 4 15 happyReduction_40
happyReduction_40 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Not happy_var_3
	) `HappyStk` happyRest

happyReduce_41 = happySpecReduce_1  15 happyReduction_41
happyReduction_41 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  16 happyReduction_42
happyReduction_42 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (Less happy_var_1 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  16 happyReduction_43
happyReduction_43 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (Equality happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  16 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn16
		 (BTrue
	)

happyReduce_45 = happySpecReduce_1  16 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn16
		 (BFalse
	)

happyReduce_46 = happySpecReduce_3  17 happyReduction_46
happyReduction_46 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Add happy_var_1 happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  17 happyReduction_47
happyReduction_47 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  17 happyReduction_48
happyReduction_48 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Mult happy_var_1 happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  17 happyReduction_49
happyReduction_49 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Div  happy_var_1 happy_var_3
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  17 happyReduction_50
happyReduction_50 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (happy_var_2
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  17 happyReduction_51
happyReduction_51 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  18 happyReduction_52
happyReduction_52 (HappyTerminal (TokenDigit _ happy_var_1))
	 =  HappyAbsSyn18
		 (Int happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  18 happyReduction_53
happyReduction_53 (HappyTerminal (TokenVar _ happy_var_1))
	 =  HappyAbsSyn18
		 (Var happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 62 62 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenMinus _ -> cont 19;
	TokenMult _ -> cont 20;
	TokenDiv _ -> cont 21;
	TokenOpBracket _ -> cont 22;
	TokenClBracket _ -> cont 23;
	TokenEq _ -> cont 24;
	TokenPlus _ -> cont 25;
	TokenLess _ -> cont 26;
	TokenEquality _ -> cont 27;
	TokenComma _ -> cont 28;
	TokenOSBracket _ -> cont 29;
	TokenClSBracket _ -> cont 30;
	TokenOCurBracket _ -> cont 31;
	TokenClCurBracket _ -> cont 32;
	TokenSemiColon _ -> cont 33;
	TokenHash _ -> cont 34;
	TokenImport _ -> cont 35;
	TokenPrint _ -> cont 36;
	TokenIf _ -> cont 37;
	TokenThen _ -> cont 38;
	TokenElse _ -> cont 39;
	TokenFor _ -> cont 40;
	TokenCreate _ -> cont 41;
	TokenBlank _ -> cont 42;
	TokenFlipX _ -> cont 43;
	TokenFlipY _ -> cont 44;
	TokenNCol _ -> cont 45;
	TokenNRow _ -> cont 46;
	TokenRotate _ -> cont 47;
	TokenSubtile _ -> cont 48;
	TokenConj _ -> cont 49;
	TokenNeg _ -> cont 50;
	TokenSupersize _ -> cont 51;
	TokenStack _ -> cont 52;
	TokenJoin _ -> cont 53;
	TokenAND _ -> cont 54;
	TokenOR _ -> cont 55;
	TokenNOT _ -> cont 56;
	TokenBTrue _ -> cont 57;
	TokenBFalse _ -> cont 58;
	TokenGroup _ happy_dollar_dollar -> cont 59;
	TokenVar _ happy_dollar_dollar -> cont 60;
	TokenDigit _ happy_dollar_dollar -> cont 61;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 62 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(TokenType)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parseCalc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [TokenType] -> a
parseError rem | null rem = error "Missing input"
               | otherwise = error ("Parse error on: " ++ (token_posn $ head rem))
parseError _ = error "Parse error" 


data Exp = Assign String Exp
         | TGroup String Exp
         | Accessor String Exp
         | For Exp Exp
         | If Exp Exp Exp
         | ElseIf Exp Exp Exp
         | Else Exp
         | Tile Exp
         | Blank Exp Exp
         | NumCol Exp
         | NumRow Exp
         | Imports String Exp
         | Print Exp
         | FlipY Exp
         | FlipX Exp
         | Join Exp
         | Stack Exp
         | Rotate Exp
         | Sub Exp Exp Exp Exp
         | Super Exp Exp
         | Conjunction Exp Exp
         | Negation Exp
         | Var String
         | Single String
         | List String Exp
         | Row Exp
         | Rows Exp Exp
         | Block Int
         | Blocks Int Exp  
         | And Exp Exp
         | Or Exp Exp
         | Not Exp
         | Less Exp Exp
         | Equality Exp Exp
         | Add Exp Exp
         | Mult Exp Exp
         | Minus Exp Exp
         | Div Exp Exp
         | Int Int
         | BTrue 
         | BFalse
         | Semis Exp Exp
         | Hash Exp
         | Hashes Exp Exp
         deriving (Show, Eq, Read)
         
data Type =   TGType
            | TileType
            | RowType
            | IntType
            | BoolType
            | BlockType
            | FunctionType Type Type
            | PairType Type Type
            | HashType Type 
            | HashList Type Type 
            | SemiList Type Type
            deriving (Show, Eq, Read)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
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
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









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
