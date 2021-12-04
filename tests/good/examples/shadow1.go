package main

import "fmt"

func main() {
	n := 0
	{
		n := 1
		if n == 1 {
			fmt.Print("a")
		}
	}
	if n == 0 {
		fmt.Print("b")
	}
	fmt.Print("\n")
}

/*
== Expected TAST ==
TDfunction_1:fn_name = "main"
root -> TDfunction_1
TDfunction_1:fn_typ -> Tvoid_2
TDfunction_1:expr -> TEblock_3
TEblock_3:expr_list -> TEblock_4
TEblock_4:expr_list -> TEvars_5
var_6:v_name = "n"; v_id = "1"; v_used = "true"; v_addr = "false"
TEvars_5:var_list -> var_6
var_6:v_typ -> Tint_7
TEblock_4:expr_list -> TEassign_8
TEassign_8:expr_list_left -> TEident_9
var_10:v_name = "n"; v_id = "1"; v_used = "true"; v_addr = "false"
TEident_9:var -> var_10
var_10:v_typ -> Tint_11
TEassign_8:expr_list_right -> TEconstant_12
Cint_13:int64 = "0"
TEconstant_12:constant -> Cint_13
TEblock_3:expr_list -> TEblock_14
TEblock_14:expr_list -> TEblock_15
TEblock_15:expr_list -> TEvars_16
var_17:v_name = "n"; v_id = "2"; v_used = "true"; v_addr = "false"
TEvars_16:var_list -> var_17
var_17:v_typ -> Tint_18
TEblock_15:expr_list -> TEassign_19
TEassign_19:expr_list_left -> TEident_20
var_21:v_name = "n"; v_id = "2"; v_used = "true"; v_addr = "false"
TEident_20:var -> var_21
var_21:v_typ -> Tint_22
TEassign_19:expr_list_right -> TEconstant_23
Cint_24:int64 = "1"
TEconstant_23:constant -> Cint_24
TEblock_14:expr_list -> TEif_25
TEbinop_26:binop = "=="
TEif_25:expr_cond -> TEbinop_26
TEbinop_26:pexpr_left -> TEident_27
var_28:v_name = "n"; v_id = "2"; v_used = "true"; v_addr = "false"
TEident_27:var -> var_28
var_28:v_typ -> Tint_29
TEbinop_26:pexpr_right -> TEconstant_30
Cint_31:int64 = "1"
TEconstant_30:constant -> Cint_31
TEif_25:expr_if -> TEblock_32
TEblock_32:expr_list -> TEprint_33
TEprint_33:expr_list -> TEconstant_34
Cstring_35:string = "a"
TEconstant_34:constant -> Cstring_35
TEif_25:expr_else -> TEskip_36
TEblock_3:expr_list -> TEif_37
TEbinop_38:binop = "=="
TEif_37:expr_cond -> TEbinop_38
TEbinop_38:pexpr_left -> TEident_39
var_40:v_name = "n"; v_id = "1"; v_used = "true"; v_addr = "false"
TEident_39:var -> var_40
var_40:v_typ -> Tint_41
TEbinop_38:pexpr_right -> TEconstant_42
Cint_43:int64 = "0"
TEconstant_42:constant -> Cint_43
TEif_37:expr_if -> TEblock_44
TEblock_44:expr_list -> TEprint_45
TEprint_45:expr_list -> TEconstant_46
Cstring_47:string = "b"
TEconstant_46:constant -> Cstring_47
TEif_37:expr_else -> TEskip_48
TEblock_3:expr_list -> TEprint_49
TEprint_49:expr_list -> TEconstant_50
Cstring_51:string = "
"
TEconstant_50:constant -> Cstring_51
*/
