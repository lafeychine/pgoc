package main

import "fmt"

func foo() (int, string, int, string) {
	return 42, "Hello", 84, "World"
}

func main() {
	nb, hello, nb2, world := foo()

	fmt.Print(nb, nb2, hello, world, "\n")
}

/*
== Expected program output ==
42 84HelloWorld
*/
