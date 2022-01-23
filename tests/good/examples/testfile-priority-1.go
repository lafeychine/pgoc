package main

import "fmt"

type T struct {
	a int
	b bool
	p *int
}

func main() {
	t := new(T)
	t.a = 42
	t.p = &t.a

	fmt.Print(-t.a, !t.b, *t.p, "\n")
}

/*
== Expected program output ==
-42 true 42
*/
