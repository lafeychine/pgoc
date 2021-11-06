package main

func f() int {
	return 42
}

/*
== Expected compiler output ==
File "./tests/bad/main/missing_main.go", line 0, characters -1--1:
error: missing method main
*/
