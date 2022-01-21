package main

import "fmt"

func main() {
	x, y := 42, 84
	fmt.Print(x, y, "\n")

	x, y = y, x + y
	fmt.Print(x, y, "\n")
}

/*
== Expected program output ==
42 84
84 126
*/
