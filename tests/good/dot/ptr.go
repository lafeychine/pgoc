package main

import "fmt"

type T struct {
	x int
}

func main() {
	t := new(T)

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
TEblock_6:expr_list -> TEblock_7
TEblock_7:expr_list -> TEvars_8
var_9:v_name = "t"; v_id = "1"; v_used = "true"; v_addr = "false"
TEvars_8:var_list -> var_9
var_9:v_typ -> Tptr_10
Tstruct_11:s_name = "T"
Tptr_10:typ -> Tstruct_11
sfield_12:f_name = "x"; f_ofs = "0"
Tstruct_11:s_fields -> sfield_12
sfield_12:f_typ -> Tint_13
TEblock_7:expr_list -> TEassign_14
TEassign_14:expr_list_left -> TEident_15
var_16:v_name = "t"; v_id = "1"; v_used = "true"; v_addr = "false"
TEident_15:var -> var_16
var_16:v_typ -> Tptr_17
Tstruct_18:s_name = "T"
Tptr_17:typ -> Tstruct_18
sfield_19:f_name = "x"; f_ofs = "0"
Tstruct_18:s_fields -> sfield_19
sfield_19:f_typ -> Tint_20
TEassign_14:expr_list_right -> TEnew_21
Tstruct_22:s_name = "T"
TEnew_21:typ -> Tstruct_22
sfield_23:f_name = "x"; f_ofs = "0"
Tstruct_22:s_fields -> sfield_23
sfield_23:f_typ -> Tint_24
TEblock_6:expr_list -> TEassign_25
TEassign_25:expr_list_left -> TEdot_26
TEdot_26:expr -> TEident_27
var_28:v_name = "t"; v_id = "1"; v_used = "true"; v_addr = "false"
TEident_27:var -> var_28
var_28:v_typ -> Tptr_29
Tstruct_30:s_name = "T"
Tptr_29:typ -> Tstruct_30
sfield_31:f_name = "x"; f_ofs = "0"
Tstruct_30:s_fields -> sfield_31
sfield_31:f_typ -> Tint_32
sfield_33:f_name = "x"; f_ofs = "0"
TEdot_26:field -> sfield_33
sfield_33:f_typ -> Tint_34
TEassign_25:expr_list_right -> TEconstant_35
Cint_36:int64 = "12"
TEconstant_35:constant -> Cint_36
TEblock_6:expr_list -> TEprint_37
TEprint_37:expr_list -> TEident_38
var_39:v_name = "t"; v_id = "1"; v_used = "true"; v_addr = "false"
TEident_38:var -> var_39
var_39:v_typ -> Tptr_40
Tstruct_41:s_name = "T"
Tptr_40:typ -> Tstruct_41
sfield_42:f_name = "x"; f_ofs = "0"
Tstruct_41:s_fields -> sfield_42
sfield_42:f_typ -> Tint_43
*/
