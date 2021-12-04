package main

import "fmt"

func foo(x int) (int, int) { return x, x + 1 }
func bar(x, y int)         { fmt.Print(x + y) }
func main()                { bar(foo(41)) }

/*
== Expected TAST ==
TDfunction_1:fn_name = "foo"
root -> TDfunction_1
var_2:v_name = "x"; v_id = "1"; v_used = "true"; v_addr = "false"
TDfunction_1:fn_params -> var_2
var_2:v_typ -> Tint_3
TDfunction_1:fn_typ -> Tmany_4
Tmany_4:typ_list -> Tint_5
Tmany_4:typ_list -> Tint_6
TDfunction_1:expr -> TEblock_7
TEblock_7:expr_list -> TEreturn_8
TEreturn_8:expr_list -> TEident_9
var_10:v_name = "x"; v_id = "1"; v_used = "true"; v_addr = "false"
TEident_9:var -> var_10
var_10:v_typ -> Tint_11
TEbinop_12:binop = "+"
TEreturn_8:expr_list -> TEbinop_12
TEbinop_12:pexpr_left -> TEident_13
var_14:v_name = "x"; v_id = "1"; v_used = "true"; v_addr = "false"
TEident_13:var -> var_14
var_14:v_typ -> Tint_15
TEbinop_12:pexpr_right -> TEconstant_16
Cint_17:int64 = "1"
TEconstant_16:constant -> Cint_17
TDfunction_18:fn_name = "bar"
root -> TDfunction_18
var_19:v_name = "x"; v_id = "2"; v_used = "true"; v_addr = "false"
TDfunction_18:fn_params -> var_19
var_19:v_typ -> Tint_20
var_21:v_name = "y"; v_id = "3"; v_used = "true"; v_addr = "false"
TDfunction_18:fn_params -> var_21
var_21:v_typ -> Tint_22
TDfunction_18:fn_typ -> Tvoid_23
TDfunction_18:expr -> TEblock_24
TEblock_24:expr_list -> TEprint_25
TEbinop_26:binop = "+"
TEprint_25:expr_list -> TEbinop_26
TEbinop_26:pexpr_left -> TEident_27
var_28:v_name = "x"; v_id = "2"; v_used = "true"; v_addr = "false"
TEident_27:var -> var_28
var_28:v_typ -> Tint_29
TEbinop_26:pexpr_right -> TEident_30
var_31:v_name = "y"; v_id = "3"; v_used = "true"; v_addr = "false"
TEident_30:var -> var_31
var_31:v_typ -> Tint_32
TDfunction_33:fn_name = "main"
root -> TDfunction_33
TDfunction_33:fn_typ -> Tvoid_34
TDfunction_33:expr -> TEblock_35
TEcall_36:fn_name = "bar"
TEblock_35:expr_list -> TEcall_36
var_37:v_name = "x"; v_id = "2"; v_used = "true"; v_addr = "false"
TEcall_36:fn_params -> var_37
var_37:v_typ -> Tint_38
var_39:v_name = "y"; v_id = "3"; v_used = "true"; v_addr = "false"
TEcall_36:fn_params -> var_39
var_39:v_typ -> Tint_40
TEcall_36:fn_typ -> Tvoid_41
TEcall_42:fn_name = "foo"
TEcall_36:expr_list -> TEcall_42
var_43:v_name = "x"; v_id = "1"; v_used = "true"; v_addr = "false"
TEcall_42:fn_params -> var_43
var_43:v_typ -> Tint_44
TEcall_42:fn_typ -> Tmany_45
Tmany_45:typ_list -> Tint_46
Tmany_45:typ_list -> Tint_47
TEcall_42:expr_list -> TEconstant_48
Cint_49:int64 = "41"
TEconstant_48:constant -> Cint_49
*/
