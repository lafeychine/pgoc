package main

import "fmt"

func foo() int {
	return 42
}

func main() {
	a := foo()

	fmt.Print(a)
}

/*
== Expected TAST ==
TDfunction_1:fn_name = "foo"
root -> TDfunction_1
TDfunction_1:fn_typ -> Tint_2
TDfunction_1:expr -> TEblock_3
TEblock_3:expr_list -> TEreturn_4
TEreturn_4:expr_list -> TEconstant_5
Cint_6:int64 = "42"
TEconstant_5:constant -> Cint_6
TDfunction_7:fn_name = "main"
root -> TDfunction_7
TDfunction_7:fn_typ -> Tvoid_8
TDfunction_7:expr -> TEblock_9
TEblock_9:expr_list -> TEblock_10
TEblock_10:expr_list -> TEvars_11
var_12:v_name = "a"; v_id = "1"; v_used = "true"; v_addr = "false"
TEvars_11:var_list -> var_12
var_12:v_typ -> Tint_13
TEblock_10:expr_list -> TEassign_14
TEassign_14:expr_list_left -> TEident_15
var_16:v_name = "a"; v_id = "1"; v_used = "true"; v_addr = "false"
TEident_15:var -> var_16
var_16:v_typ -> Tint_17
TEcall_18:fn_name = "foo"
TEassign_14:expr_list_right -> TEcall_18
TEcall_18:fn_typ -> Tint_19
TEblock_9:expr_list -> TEprint_20
TEprint_20:expr_list -> TEident_21
var_22:v_name = "a"; v_id = "1"; v_used = "true"; v_addr = "false"
TEident_21:var -> var_22
var_22:v_typ -> Tint_23
*/
