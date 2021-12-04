package main

func main() {
	12 == nil
}

/*
== Expected compiler output ==
File "./tests/bad/binop/equal_nil_on_nonptr.go", line 4, characters 1-3:
error: invalid operation: == with nil must compare a pointer, have int
*/
