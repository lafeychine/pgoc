package main

import "fmt"

func foo() int {
	return 42
}

func main() {
	nb := foo()

	fmt.Print(nb, "\n")
}

/*
== Expected program output ==
42
*/
