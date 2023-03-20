%--------------------------- NO OLVIDAR USAR LA FLAG: set_prolog_flag(report_cpu, true). -------------------
:-use_module(library(statistics)).



%----------- TEST FACIL -------------------
test_facil0:- sudoku_facil00(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil1:- sudoku_facil01(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil2:- sudoku_facil02(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil3:- sudoku_facil03(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil4:- sudoku_facil04(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil5:- sudoku_facil05(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil6:- sudoku_facil06(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil7:- sudoku_facil07(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil8:- sudoku_facil08(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil9:- sudoku_facil09(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil10:- sudoku_facil10(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil11:- sudoku_facil11(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil12:- sudoku_facil12(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil13:- sudoku_facil13(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil14:- sudoku_facil14(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil15:- sudoku_facil15(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil16:- sudoku_facil16(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil17:- sudoku_facil17(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil18:- sudoku_facil18(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil19:- sudoku_facil19(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil20:- sudoku_facil20(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil21:- sudoku_facil21(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil22:- sudoku_facil22(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil23:- sudoku_facil23(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil24:- sudoku_facil24(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil25:- sudoku_facil25(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil26:- sudoku_facil26(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil27:- sudoku_facil27(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil28:- sudoku_facil28(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil29:- sudoku_facil29(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil30:- sudoku_facil30(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil31:- sudoku_facil31(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil32:- sudoku_facil32(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil33:- sudoku_facil33(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil34:- sudoku_facil34(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil35:- sudoku_facil35(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil36:- sudoku_facil36(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil37:- sudoku_facil37(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil38:- sudoku_facil38(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil39:- sudoku_facil39(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil40:- sudoku_facil40(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil41:- sudoku_facil41(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil42:- sudoku_facil42(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil43:- sudoku_facil43(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil44:- sudoku_facil44(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil45:- sudoku_facil45(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil46:- sudoku_facil46(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil47:- sudoku_facil47(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil48:- sudoku_facil48(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil49:- sudoku_facil49(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil50:- sudoku_facil50(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil51:- sudoku_facil51(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil52:- sudoku_facil52(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil53:- sudoku_facil53(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil54:- sudoku_facil54(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil55:- sudoku_facil55(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil56:- sudoku_facil56(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil57:- sudoku_facil57(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil58:- sudoku_facil58(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil59:- sudoku_facil59(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil60:- sudoku_facil60(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil61:- sudoku_facil61(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil62:- sudoku_facil62(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil63:- sudoku_facil63(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil64:- sudoku_facil64(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil65:- sudoku_facil65(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil66:- sudoku_facil66(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil67:- sudoku_facil67(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil68:- sudoku_facil68(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil69:- sudoku_facil69(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil70:- sudoku_facil70(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil71:- sudoku_facil71(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil72:- sudoku_facil72(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil73:- sudoku_facil73(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil74:- sudoku_facil74(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil75:- sudoku_facil75(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil76:- sudoku_facil76(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil77:- sudoku_facil77(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil78:- sudoku_facil78(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil79:- sudoku_facil79(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil80:- sudoku_facil80(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil81:- sudoku_facil81(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil82:- sudoku_facil82(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil83:- sudoku_facil83(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil84:- sudoku_facil84(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil85:- sudoku_facil85(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil86:- sudoku_facil86(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil87:- sudoku_facil87(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil88:- sudoku_facil88(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil89:- sudoku_facil89(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil90:- sudoku_facil90(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil91:- sudoku_facil91(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil92:- sudoku_facil92(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil93:- sudoku_facil93(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil94:- sudoku_facil94(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil95:- sudoku_facil95(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil96:- sudoku_facil96(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil97:- sudoku_facil97(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil98:- sudoku_facil98(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_facil99:- sudoku_facil99(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
%-------------------------------------------------------------------------------


%----------- TEST MEDIOS -------------------
test_medio0:- sudoku_medio00(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio1:- sudoku_medio01(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio2:- sudoku_medio02(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio3:- sudoku_medio03(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio4:- sudoku_medio04(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio5:- sudoku_medio05(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio6:- sudoku_medio06(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio7:- sudoku_medio07(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio8:- sudoku_medio08(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio9:- sudoku_medio09(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio10:- sudoku_medio10(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio11:- sudoku_medio11(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio12:- sudoku_medio12(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio13:- sudoku_medio13(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio14:- sudoku_medio14(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio15:- sudoku_medio15(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio16:- sudoku_medio16(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio17:- sudoku_medio17(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio18:- sudoku_medio18(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio19:- sudoku_medio19(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio20:- sudoku_medio20(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio21:- sudoku_medio21(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio22:- sudoku_medio22(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio23:- sudoku_medio23(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio24:- sudoku_medio24(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio25:- sudoku_medio25(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio26:- sudoku_medio26(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio27:- sudoku_medio27(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio28:- sudoku_medio28(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio29:- sudoku_medio29(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio30:- sudoku_medio30(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio31:- sudoku_medio31(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio32:- sudoku_medio32(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio33:- sudoku_medio33(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio34:- sudoku_medio34(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio35:- sudoku_medio35(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio36:- sudoku_medio36(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio37:- sudoku_medio37(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio38:- sudoku_medio38(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio39:- sudoku_medio39(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio40:- sudoku_medio40(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio41:- sudoku_medio41(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio42:- sudoku_medio42(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio43:- sudoku_medio43(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio44:- sudoku_medio44(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio45:- sudoku_medio45(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio46:- sudoku_medio46(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio47:- sudoku_medio47(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio48:- sudoku_medio48(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio49:- sudoku_medio49(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio50:- sudoku_medio50(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio51:- sudoku_medio51(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio52:- sudoku_medio52(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio53:- sudoku_medio53(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio54:- sudoku_medio54(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio55:- sudoku_medio55(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio56:- sudoku_medio56(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio57:- sudoku_medio57(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio58:- sudoku_medio58(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio59:- sudoku_medio59(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio60:- sudoku_medio60(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio61:- sudoku_medio61(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio62:- sudoku_medio62(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio63:- sudoku_medio63(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio64:- sudoku_medio64(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio65:- sudoku_medio65(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio66:- sudoku_medio66(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio67:- sudoku_medio67(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio68:- sudoku_medio68(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio69:- sudoku_medio69(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio70:- sudoku_medio70(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio71:- sudoku_medio71(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio72:- sudoku_medio72(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio73:- sudoku_medio73(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio74:- sudoku_medio74(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio75:- sudoku_medio75(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio76:- sudoku_medio76(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio77:- sudoku_medio77(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio78:- sudoku_medio78(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio79:- sudoku_medio79(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio80:- sudoku_medio80(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio81:- sudoku_medio81(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio82:- sudoku_medio82(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio83:- sudoku_medio83(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio84:- sudoku_medio84(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio85:- sudoku_medio85(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio86:- sudoku_medio86(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio87:- sudoku_medio87(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio88:- sudoku_medio88(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio89:- sudoku_medio89(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio90:- sudoku_medio90(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio91:- sudoku_medio91(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio92:- sudoku_medio92(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio93:- sudoku_medio93(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio94:- sudoku_medio94(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio95:- sudoku_medio95(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio96:- sudoku_medio96(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio97:- sudoku_medio97(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio98:- sudoku_medio98(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_medio99:- sudoku_medio99(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).


%-------------------------------------------------------------------------------
%----------- TEST DIFICILES -------------------
test_hard0:- sudoku_hard000(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard1:- sudoku_hard001(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard2:- sudoku_hard002(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard3:- sudoku_hard003(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard4:- sudoku_hard004(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard5:- sudoku_hard005(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard6:- sudoku_hard006(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard7:- sudoku_hard007(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard8:- sudoku_hard008(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard9:- sudoku_hard009(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard10:- sudoku_hard010(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard11:- sudoku_hard011(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard12:- sudoku_hard012(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard13:- sudoku_hard013(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard14:- sudoku_hard014(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard15:- sudoku_hard015(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard16:- sudoku_hard016(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard17:- sudoku_hard017(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard18:- sudoku_hard018(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard19:- sudoku_hard019(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard20:- sudoku_hard020(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard21:- sudoku_hard021(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard22:- sudoku_hard022(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard23:- sudoku_hard023(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard24:- sudoku_hard024(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard25:- sudoku_hard025(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard26:- sudoku_hard026(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard27:- sudoku_hard027(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard28:- sudoku_hard028(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard29:- sudoku_hard029(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard30:- sudoku_hard030(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard31:- sudoku_hard031(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard32:- sudoku_hard032(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard33:- sudoku_hard033(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard34:- sudoku_hard034(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard35:- sudoku_hard035(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard36:- sudoku_hard036(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard37:- sudoku_hard037(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard38:- sudoku_hard038(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard39:- sudoku_hard039(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard40:- sudoku_hard040(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard41:- sudoku_hard041(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard42:- sudoku_hard042(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard43:- sudoku_hard043(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard44:- sudoku_hard044(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard45:- sudoku_hard045(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard46:- sudoku_hard046(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard47:- sudoku_hard047(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard48:- sudoku_hard048(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard49:- sudoku_hard049(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard50:- sudoku_hard050(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard51:- sudoku_hard051(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard52:- sudoku_hard052(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard53:- sudoku_hard053(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard54:- sudoku_hard054(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard55:- sudoku_hard055(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard56:- sudoku_hard056(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard57:- sudoku_hard057(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard58:- sudoku_hard058(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard59:- sudoku_hard059(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard60:- sudoku_hard060(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard61:- sudoku_hard061(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard62:- sudoku_hard062(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard63:- sudoku_hard063(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard64:- sudoku_hard064(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard65:- sudoku_hard065(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard66:- sudoku_hard066(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard67:- sudoku_hard067(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard68:- sudoku_hard068(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard69:- sudoku_hard069(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard70:- sudoku_hard070(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard71:- sudoku_hard071(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard72:- sudoku_hard072(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard73:- sudoku_hard073(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard74:- sudoku_hard074(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard75:- sudoku_hard075(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard76:- sudoku_hard076(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard77:- sudoku_hard077(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard78:- sudoku_hard078(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard79:- sudoku_hard079(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard80:- sudoku_hard080(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard81:- sudoku_hard081(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard82:- sudoku_hard082(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard83:- sudoku_hard083(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard84:- sudoku_hard084(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard85:- sudoku_hard085(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard86:- sudoku_hard086(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard87:- sudoku_hard087(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard88:- sudoku_hard088(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard89:- sudoku_hard089(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard90:- sudoku_hard090(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard91:- sudoku_hard091(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard92:- sudoku_hard092(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard93:- sudoku_hard093(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard94:- sudoku_hard094(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard95:- sudoku_hard095(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard96:- sudoku_hard096(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard97:- sudoku_hard097(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard98:- sudoku_hard098(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
test_hard99:- sudoku_hard099(S),buscar_posibilidades(S, S1), buscar(S1, 10, _).
%----------------------------------------------------------------------------


%-------------- CONSULTAS -----------------------------------------
%LAS CONSULTAS DE TIME APARECEN COMENTADAS PUESTO QUE NO SE PUEDEN CARGAR EN UN DOCUMENTO,
%SOLO PUEDEN SER USADAS EN CONSOLA.
%LA FLAG NECESARIA A PONER EN EL DOCUMENTO, ES NECESARIO PONERLO POR CONSOLA TAMBIEN, NO SE PUEDE EN EL DOCUMENTO

%time(test_facil0),time(test_facil1),time(test_facil2),time(test_facil3),time(test_facil4),time(test_facil5),time(test_facil6),time(test_facil7),time(test_facil8),time(test_facil9),time(test_facil10),
%time(test_facil11),time(test_facil12),time(test_facil13),time(test_facil14),time(test_facil15),time(test_facil16),time(test_facil17),time(test_facil18),time(test_facil19),time(test_facil20),
%time(test_facil21),time(test_facil22),time(test_facil23),time(test_facil24),time(test_facil25),time(test_facil26),time(test_facil27),time(test_facil28),time(test_facil29),time(test_facil30),
%time(test_facil31),time(test_facil32),time(test_facil33),time(test_facil34),time(test_facil35),time(test_facil36),time(test_facil37),time(test_facil38),time(test_facil39),time(test_facil40),
%time(test_facil41),time(test_facil42),time(test_facil43),time(test_facil44),time(test_facil45),time(test_facil46),time(test_facil47),time(test_facil48),time(test_facil49),time(test_facil50),
%time(test_facil51),time(test_facil52),time(test_facil53),time(test_facil54),time(test_facil55),time(test_facil56),time(test_facil57),time(test_facil58),time(test_facil59),time(test_facil60),
%time(test_facil61),time(test_facil62),time(test_facil63),time(test_facil64),time(test_facil65),time(test_facil66),time(test_facil67),time(test_facil68),time(test_facil69),time(test_facil70),
%time(test_facil71),time(test_facil72),time(test_facil73),time(test_facil74),time(test_facil75),time(test_facil76),time(test_facil77),time(test_facil78),time(test_facil79),time(test_facil80),
%time(test_facil81),time(test_facil82),time(test_facil83),time(test_facil84),time(test_facil85),time(test_facil86),time(test_facil87),time(test_facil88),time(test_facil89),time(test_facil90),
%time(test_facil91),time(test_facil92),time(test_facil93),time(test_facil94),time(test_facil95),time(test_facil96),time(test_facil97),time(test_facil98),time(test_facil99).

%time(test_medio0),time(test_medio1),time(test_medio2),time(test_medio3),time(test_medio4),time(test_medio5),time(test_medio6),time(test_medio7),time(test_medio8),time(test_medio9),time(test_medio10),
%time(test_medio11),time(test_medio12),time(test_medio13),time(test_medio14),time(test_medio15),time(test_medio16),time(test_medio17),time(test_medio18),time(test_medio19),time(test_medio20),
%time(test_medio21),time(test_medio22),time(test_medio23),time(test_medio24),time(test_medio25),time(test_medio26),time(test_medio27),time(test_medio28),time(test_medio29),time(test_medio30),
%time(test_medio31),time(test_medio32),time(test_medio33),time(test_medio34),time(test_medio35),time(test_medio36),time(test_medio37),time(test_medio38),time(test_medio39),time(test_medio40),
%time(test_medio41),time(test_medio42),time(test_medio43),time(test_medio44),time(test_medio45),time(test_medio46),time(test_medio47),time(test_medio48),time(test_medio49),time(test_medio50),
%time(test_medio51),time(test_medio52),time(test_medio53),time(test_medio54),time(test_medio55),time(test_medio56),time(test_medio57),time(test_medio58),time(test_medio59),time(test_medio60),
%time(test_medio61),time(test_medio62),time(test_medio63),time(test_medio64),time(test_medio65),time(test_medio66),time(test_medio67),time(test_medio68),time(test_medio69),time(test_medio70),
%time(test_medio71),time(test_medio72),time(test_medio73),time(test_medio74),time(test_medio75),time(test_medio76),time(test_medio77),time(test_medio78),time(test_medio79),time(test_medio80),
%time(test_medio81),time(test_medio82),time(test_medio83),time(test_medio84),time(test_medio85),time(test_medio86),time(test_medio87),time(test_medio88),time(test_medio89),time(test_medio90),
%time(test_medio91),time(test_medio92),time(test_medio93),time(test_medio94),time(test_medio95),time(test_medio96),time(test_medio97),time(test_medio98),time(test_medio99).

%time(test_hard0),time(test_hard1),time(test_hard2),time(test_hard3),time(test_hard4),time(test_hard5),time(test_hard6),time(test_hard7),time(test_hard8),time(test_hard9),time(test_hard10),
%time(test_hard11),time(test_hard12),time(test_hard13),time(test_hard14),time(test_hard15),time(test_hard16),time(test_hard17),time(test_hard18),time(test_hard19),time(test_hard20),
%time(test_hard21),time(test_hard22),time(test_hard23),time(test_hard24),time(test_hard25),time(test_hard26),time(test_hard27),time(test_hard28),time(test_hard29),time(test_hard30),
%time(test_hard31),time(test_hard32),time(test_hard33),time(test_hard34),time(test_hard35),time(test_hard36),time(test_hard37),time(test_hard38),time(test_hard39),time(test_hard40),
%time(test_hard41),time(test_hard42),time(test_hard43),time(test_hard44),time(test_hard45),time(test_hard46),time(test_hard47),time(test_hard48),time(test_hard49),time(test_hard50),
%time(test_hard51),time(test_hard52),time(test_hard53),time(test_hard54),time(test_hard55),time(test_hard56),time(test_hard57),time(test_hard58),time(test_hard59),time(test_hard60),
%time(test_hard61),time(test_hard62),time(test_hard63),time(test_hard64),time(test_hard65),time(test_hard66),time(test_hard67),time(test_hard68),time(test_hard69),time(test_hard70),
%time(test_hard71),time(test_hard72),time(test_hard73),time(test_hard74),time(test_hard75),time(test_hard76),time(test_hard77),time(test_hard78),time(test_hard79),time(test_hard80),
%time(test_hard81),time(test_hard82),time(test_hard83),time(test_hard84),time(test_hard85),time(test_hard86),time(test_hard87),time(test_hard88),time(test_hard89),time(test_hard90),
%time(test_hard91),time(test_hard92),time(test_hard93),time(test_hard94),time(test_hard95),time(test_hard96),time(test_hard97),time(test_hard98),time(test_hard99).
