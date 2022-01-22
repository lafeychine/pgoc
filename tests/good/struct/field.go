package main

import "fmt"

type L struct {
	x int
	y string
}

func main() {
	var l L

	l.x = 2
	l.y = "Hello"

	fmt.Print(l.x, l.y, "\n")
	fmt.Print(l, "\n")
}

/*
== Expected program output ==
2Hello
{2 Hello}
*/
