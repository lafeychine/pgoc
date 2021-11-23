package main

import "fmt"

func main() {
	fmt.Print("a")
}

/*
== Expected TAST ==
TDfunction_1:fn_name = "main"
root -> TDfunction_1
TDfunction_1:fn_typ -> Tvoid_2
TDfunction_1:expr -> TEblock_3
TEblock_3:expr_list -> TEprint_4
TEprint_4:expr_list -> TEconstant_5
Cstring_6:string = "a"
TEconstant_5:constant -> Cstring_6
*/
