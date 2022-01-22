package main

import "fmt"

type T struct {
	x, y int
}

func main() {
	t := new(T)
	fmt.Print(t.y, "\n")

	t.x = 1
	p := &t.x
	fmt.Print(*p, "\n")

	*p = 2
	fmt.Print(*p, "\n")
}

/*
== Expected program output ==
0
1
2
*/
