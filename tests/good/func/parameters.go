package main

import "fmt"

func foo(a int) {
	fmt.Print(a)
}

func main() {
	foo(42)
}

/*
== Expected TAST ==
TDfunction_1:fn_name = "foo"
root -> TDfunction_1
var_2:v_name = "a"; v_id = "1"; v_used = "true"; v_addr = "false"
TDfunction_1:fn_params -> var_2
var_2:v_typ -> Tint_3
TDfunction_1:fn_typ -> Tvoid_4
TDfunction_1:expr -> TEblock_5
TEblock_5:expr_list -> TEprint_6
TEprint_6:expr_list -> TEident_7
var_8:v_name = "a"; v_id = "1"; v_used = "true"; v_addr = "false"
TEident_7:var -> var_8
var_8:v_typ -> Tint_9
TDfunction_10:fn_name = "main"
root -> TDfunction_10
TDfunction_10:fn_typ -> Tvoid_11
TDfunction_10:expr -> TEblock_12
TEcall_13:fn_name = "foo"
TEblock_12:expr_list -> TEcall_13
var_14:v_name = "a"; v_id = "1"; v_used = "true"; v_addr = "false"
TEcall_13:fn_params -> var_14
var_14:v_typ -> Tint_15
TEcall_13:fn_typ -> Tvoid_16
TEcall_13:expr_list -> TEconstant_17
Cint_18:int64 = "42"
TEconstant_17:constant -> Cint_18
*/
