package main

func foo(a int) {}

func main() {
	foo(1, 2)
}

/*
== Expected compiler output ==
File "./tests/bad/call/mismatch_params_number.go", line 6, characters 1-4:
error: incorrect number of arguments in call to foo: 2 arguments but 1 parameters
*/
