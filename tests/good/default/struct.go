package main

import "fmt"

type L struct {
	x int
	y string
	z *L
}

func main() {
	var l L

	fmt.Print(l, "\n")
}

/*
== Expected program output ==
{0  <nil>}
*/
