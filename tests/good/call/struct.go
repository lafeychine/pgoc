package main

import "fmt"

type L struct {
	x int
	y int
}

func foo(l L) {
	fmt.Print(l, "\n")
}

func main() {
	var l L

	l.x = 42
	l.y = 84

	foo(l)
}

/*
== Expected program output ==
{42 84}
*/
