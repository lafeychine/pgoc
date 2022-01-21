package main

import "fmt"

func foo(x int) (int, int) {
	return x, x + 1
}

func bar(x, y int) {
	fmt.Print(x + y, "\n")
}

func main() {
	bar(foo(41))
}

/*
== Expected program output ==
83
*/
