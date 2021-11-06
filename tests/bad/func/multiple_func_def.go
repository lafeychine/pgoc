package main

func f() {}
func f() {}

/*
== Expected compiler output ==
File "./tests/bad/func/multiple_func_def.go", line 4, characters 5-6:
error: function f redeclared
*/
