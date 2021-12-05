package main

import "fmt"

func swap(a, b int) (int, int) {
	return b, a
}

func main() {
	var a int

	a, _ = swap(2, 3)

	fmt.Print(a)
}

/*
== Expected TAST ==
TDfunction_1:fn_name = "swap"
root -> TDfunction_1
var_2:v_name = "a"; v_id = "1"; v_used = "true"; v_addr = "false"
TDfunction_1:fn_params -> var_2
var_2:v_typ -> Tint_3
var_4:v_name = "b"; v_id = "2"; v_used = "true"; v_addr = "false"
TDfunction_1:fn_params -> var_4
var_4:v_typ -> Tint_5
TDfunction_1:fn_typ -> Tmany_6
Tmany_6:typ_list -> Tint_7
Tmany_6:typ_list -> Tint_8
TDfunction_1:expr -> TEblock_9
TEblock_9:expr_list -> TEreturn_10
TEreturn_10:expr_list -> TEident_11
var_12:v_name = "b"; v_id = "2"; v_used = "true"; v_addr = "false"
TEident_11:var -> var_12
var_12:v_typ -> Tint_13
TEreturn_10:expr_list -> TEident_14
var_15:v_name = "a"; v_id = "1"; v_used = "true"; v_addr = "false"
TEident_14:var -> var_15
var_15:v_typ -> Tint_16
TDfunction_17:fn_name = "main"
root -> TDfunction_17
TDfunction_17:fn_typ -> Tvoid_18
TDfunction_17:expr -> TEblock_19
TEblock_19:expr_list -> TEvars_20
var_21:v_name = "a"; v_id = "3"; v_used = "true"; v_addr = "false"
TEvars_20:var_list -> var_21
var_21:v_typ -> Tint_22
TEblock_19:expr_list -> TEassign_23
TEassign_23:expr_list_left -> TEident_24
var_25:v_name = "a"; v_id = "3"; v_used = "true"; v_addr = "false"
TEident_24:var -> var_25
var_25:v_typ -> Tint_26
TEassign_23:expr_list_left -> TEident_27
var_28:v_name = "_"; v_id = "0"; v_used = "true"; v_addr = "false"
TEident_27:var -> var_28
var_28:v_typ -> Tvoid_29
TEcall_30:fn_name = "swap"
TEassign_23:expr_list_right -> TEcall_30
var_31:v_name = "a"; v_id = "1"; v_used = "true"; v_addr = "false"
TEcall_30:fn_params -> var_31
var_31:v_typ -> Tint_32
var_33:v_name = "b"; v_id = "2"; v_used = "true"; v_addr = "false"
TEcall_30:fn_params -> var_33
var_33:v_typ -> Tint_34
TEcall_30:fn_typ -> Tmany_35
Tmany_35:typ_list -> Tint_36
Tmany_35:typ_list -> Tint_37
TEcall_30:expr_list -> TEconstant_38
Cint_39:int64 = "2"
TEconstant_38:constant -> Cint_39
TEcall_30:expr_list -> TEconstant_40
Cint_41:int64 = "3"
TEconstant_40:constant -> Cint_41
TEblock_19:expr_list -> TEprint_42
TEprint_42:expr_list -> TEident_43
var_44:v_name = "a"; v_id = "3"; v_used = "true"; v_addr = "false"
TEident_43:var -> var_44
var_44:v_typ -> Tint_45
*/
