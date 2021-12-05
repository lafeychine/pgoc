package main

import "fmt"

func swap(a, b int) (int, int) {
	return b, a
}

func main() {
	var a, b int

	a, b = swap(2, 3)

	fmt.Print(a, b)
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
var_23:v_name = "b"; v_id = "4"; v_used = "true"; v_addr = "false"
TEvars_20:var_list -> var_23
var_23:v_typ -> Tint_24
TEblock_19:expr_list -> TEassign_25
TEassign_25:expr_list_left -> TEident_26
var_27:v_name = "a"; v_id = "3"; v_used = "true"; v_addr = "false"
TEident_26:var -> var_27
var_27:v_typ -> Tint_28
TEassign_25:expr_list_left -> TEident_29
var_30:v_name = "b"; v_id = "4"; v_used = "true"; v_addr = "false"
TEident_29:var -> var_30
var_30:v_typ -> Tint_31
TEcall_32:fn_name = "swap"
TEassign_25:expr_list_right -> TEcall_32
var_33:v_name = "a"; v_id = "1"; v_used = "true"; v_addr = "false"
TEcall_32:fn_params -> var_33
var_33:v_typ -> Tint_34
var_35:v_name = "b"; v_id = "2"; v_used = "true"; v_addr = "false"
TEcall_32:fn_params -> var_35
var_35:v_typ -> Tint_36
TEcall_32:fn_typ -> Tmany_37
Tmany_37:typ_list -> Tint_38
Tmany_37:typ_list -> Tint_39
TEcall_32:expr_list -> TEconstant_40
Cint_41:int64 = "2"
TEconstant_40:constant -> Cint_41
TEcall_32:expr_list -> TEconstant_42
Cint_43:int64 = "3"
TEconstant_42:constant -> Cint_43
TEblock_19:expr_list -> TEprint_44
TEprint_44:expr_list -> TEident_45
var_46:v_name = "a"; v_id = "3"; v_used = "true"; v_addr = "false"
TEident_45:var -> var_46
var_46:v_typ -> Tint_47
TEprint_44:expr_list -> TEident_48
var_49:v_name = "b"; v_id = "4"; v_used = "true"; v_addr = "false"
TEident_48:var -> var_49
var_49:v_typ -> Tint_50
*/
