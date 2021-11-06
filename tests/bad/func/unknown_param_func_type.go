package main

func f(a A) {}

/*
== Expected compiler output ==
File "./tests/bad/func/unknown_param_func_type.go", line 3, characters 7-8:
error: undefined type A of parameter a in function f
*/
