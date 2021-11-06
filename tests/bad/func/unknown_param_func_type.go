package main

func foo(a A) {}

/*
== Expected compiler output ==
File "./tests/bad/func/unknown_param_func_type.go", line 3, characters 9-10:
error: undefined type of parameter a in function foo
*/
