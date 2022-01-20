package main

import "fmt"

func foo() (int, string) {
	return 42, "Hello"
}

func main() {
	nb, str := foo()

	fmt.Print(nb, str, "\n")
}

/*
== Expected program output ==
42Hello
*/
