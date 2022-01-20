package main

import "fmt"

func main() {
	x := 2

	x++
	x++
	fmt.Print(x, "\n")

	x--
	fmt.Print(x, "\n")
}

/*
== Expected program output ==
4
3
*/
