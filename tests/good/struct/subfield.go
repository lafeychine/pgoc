package main

import "fmt"

type L struct {
	x int
	y M
}

type M struct {
	x int
	y string
}

func main() {
	var l L

	l.x = 2
	l.y.x = 3
	l.y.y = "Hello"

	fmt.Print(l.x, l.y.x, l.y.y, "\n")
}

/*
== Expected program output ==
2 3Hello
*/
