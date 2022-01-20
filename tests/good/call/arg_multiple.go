package main

import "fmt"

func foo(nb int, str string) {
	fmt.Print(nb, str, "\n")
}

func main() {
	foo(42, "Hello")
}

/*
== Expected program output ==
42Hello
*/
