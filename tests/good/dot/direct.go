package main

import "fmt"

type T struct {
	x int
}

func main() {
	var t T

	t.x = 12
	fmt.Print(t)
}

/*
== Expected TAST ==
TDstruct_1:s_name = "T"
root -> TDstruct_1
sfield_2:f_name = "x"; f_ofs = "0"
TDstruct_1:s_fields -> sfield_2
sfield_2:f_typ -> Tint_3
TDfunction_4:fn_name = "main"
root -> TDfunction_4
TDfunction_4:fn_typ -> Tvoid_5
TDfunction_4:expr -> TEblock_6
TEblock_6:expr_list -> TEvars_7
var_8:v_name = "t"; v_id = "1"; v_used = "true"; v_addr = "false"
TEvars_7:var_list -> var_8
Tstruct_9:s_name = "T"
var_8:v_typ -> Tstruct_9
sfield_10:f_name = "x"; f_ofs = "0"
Tstruct_9:s_fields -> sfield_10
sfield_10:f_typ -> Tint_11
TEblock_6:expr_list -> TEassign_12
TEassign_12:expr_list_left -> TEdot_13
TEdot_13:expr -> TEident_14
var_15:v_name = "t"; v_id = "1"; v_used = "true"; v_addr = "false"
TEident_14:var -> var_15
Tstruct_16:s_name = "T"
var_15:v_typ -> Tstruct_16
sfield_17:f_name = "x"; f_ofs = "0"
Tstruct_16:s_fields -> sfield_17
sfield_17:f_typ -> Tint_18
sfield_19:f_name = "x"; f_ofs = "0"
TEdot_13:field -> sfield_19
sfield_19:f_typ -> Tint_20
TEassign_12:expr_list_right -> TEconstant_21
Cint_22:int64 = "12"
TEconstant_21:constant -> Cint_22
TEblock_6:expr_list -> TEprint_23
TEprint_23:expr_list -> TEident_24
var_25:v_name = "t"; v_id = "1"; v_used = "true"; v_addr = "false"
TEident_24:var -> var_25
Tstruct_26:s_name = "T"
var_25:v_typ -> Tstruct_26
sfield_27:f_name = "x"; f_ofs = "0"
Tstruct_26:s_fields -> sfield_27
sfield_27:f_typ -> Tint_28
*/
