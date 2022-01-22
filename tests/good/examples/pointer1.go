package main

import "fmt"

func main() {
	x := 1
	p := &x

	fmt.Print(*p, "\n")

	*p = 2
	fmt.Print(*p, "\n")
}

/*
== Expected program output ==
1
2
*/
