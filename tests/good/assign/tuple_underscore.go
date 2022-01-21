package main

import "fmt"

func swap(a, b int) (int, int) {
	return b, a
}

func main() {
	a, _ := swap(42, 84)
	fmt.Print(a, "\n")

	_, b := swap(42, 84)
	fmt.Print(b, "\n")
}

/*
== Expected program output ==
84
42
*/
