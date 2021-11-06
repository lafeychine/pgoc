package main

func toto(a int, a int) {}

/*
== Expected compiler output ==
File "./tests/bad/func/multiple_param_func_def.go", line 3, characters 10-11:
error: duplicate parameter a in function toto
*/
