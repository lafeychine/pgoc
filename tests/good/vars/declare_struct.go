package main

import "fmt"

type A struct {
	x int
}

func main() {
	var a A

	fmt.Print(a.x)
}

/*
== Expected TAST ==
TDstruct_1:s_name = "A"
root -> TDstruct_1
sfield_2:f_name = "x"; f_ofs = "0"
TDstruct_1:s_fields -> sfield_2
sfield_2:f_typ -> Tint_3
TDfunction_4:fn_name = "main"
root -> TDfunction_4
TDfunction_4:fn_typ -> Tvoid_5
TDfunction_4:expr -> TEblock_6
TEblock_6:expr_list -> TEvars_7
var_8:v_name = "a"; v_id = "1"; v_used = "true"; v_addr = "false"
TEvars_7:var_list -> var_8
Tstruct_9:s_name = "A"
var_8:v_typ -> Tstruct_9
sfield_10:f_name = "x"; f_ofs = "0"
Tstruct_9:s_fields -> sfield_10
sfield_10:f_typ -> Tint_11
TEblock_6:expr_list -> TEprint_12
TEprint_12:expr_list -> TEdot_13
TEdot_13:expr -> TEident_14
var_15:v_name = "a"; v_id = "1"; v_used = "true"; v_addr = "false"
TEident_14:var -> var_15
Tstruct_16:s_name = "A"
var_15:v_typ -> Tstruct_16
sfield_17:f_name = "x"; f_ofs = "0"
Tstruct_16:s_fields -> sfield_17
sfield_17:f_typ -> Tint_18
sfield_19:f_name = "x"; f_ofs = "0"
TEdot_13:field -> sfield_19
sfield_19:f_typ -> Tint_20
*/
