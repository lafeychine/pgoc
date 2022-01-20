package main

import "fmt"

func foo(nb int) {
	fmt.Print(nb, "\n")
}

func main() {
	foo(42)
}

/*
== Expected program output ==
42
*/
