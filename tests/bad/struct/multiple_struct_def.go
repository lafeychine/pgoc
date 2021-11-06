package main

type Toto struct {
	x int
}

type Toto struct {
	x int
}

/*
== Expected compiler output ==
File "./tests/bad/struct/multiple_struct_def.go", line 7, characters 5-9:
error: structure Toto redeclared
*/
