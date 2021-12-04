package main

import "fmt"

func main() {
	fmt.Print(12)
}

/*
== Expected TAST ==
TDfunction_1:fn_name = "main"
root -> TDfunction_1
TDfunction_1:fn_typ -> Tvoid_2
TDfunction_1:expr -> TEblock_3
TEblock_3:expr_list -> TEprint_4
TEprint_4:expr_list -> TEconstant_5
Cint_6:int64 = "12"
TEconstant_5:constant -> Cint_6
*/
