package main

import "fmt"

func sum(a int, b int) int {
	return a + b
}

func main() {
	x := sum(12, 24)

	fmt.Print(x, "\n")
}

/*
== Expected program output ==
36
*/
