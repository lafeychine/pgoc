package main

import "fmt"

func main() {
	var a int

	fmt.Print(a)
}

/*
== Expected TAST ==
TDfunction_1:fn_name = "main"
root -> TDfunction_1
TDfunction_1:fn_typ -> Tvoid_2
TDfunction_1:expr -> TEblock_3
TEblock_3:expr_list -> TEvars_4
var_5:v_name = "a"; v_id = "1"; v_used = "false"; v_addr = "false"
TEvars_4:var_list -> var_5
var_5:v_typ -> Tint_6
TEblock_3:expr_list -> TEprint_7
TEprint_7:expr_list -> TEident_8
var_9:v_name = "a"; v_id = "1"; v_used = "false"; v_addr = "false"
TEident_8:var -> var_9
var_9:v_typ -> Tint_10
*/
