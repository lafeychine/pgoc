package main

import "fmt"

func main() {
	fmt.Print(false)
	fmt.Print(true)
}

/*
== Expected TAST ==
TDfunction_1:fn_name = "main"
root -> TDfunction_1
TDfunction_1:fn_typ -> Tvoid_2
TDfunction_1:expr -> TEblock_3
TEblock_3:expr_list -> TEprint_4
TEprint_4:expr_list -> TEconstant_5
Cbool_6:bool = "false"
TEconstant_5:constant -> Cbool_6
TEblock_3:expr_list -> TEprint_7
TEprint_7:expr_list -> TEconstant_8
Cbool_9:bool = "true"
TEconstant_8:constant -> Cbool_9
*/
