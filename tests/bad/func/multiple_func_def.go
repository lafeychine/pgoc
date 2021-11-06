package main

func toto() {}
func toto() {}

/*
== Expected compiler output ==
File "./tests/bad/func/multiple_func_def.go", line 4, characters 5-9:
error: function toto redeclared
*/
