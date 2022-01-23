package main

import "fmt"

type L struct {
	lx int
	ly *M
	lz int
}

type M struct {
	mx int
	my int
}

func main() {
	var l L

	l.lx = 2
	l.ly = new(M)
	l.ly.mx = 3
	l.ly.my = 4
	l.lz = 5

	x := l

	fmt.Print(l.lx, l.ly.mx, l.ly.my, l.lz, "\n")
	fmt.Print(x.lx, x.ly.mx, x.ly.my, x.lz, "\n")

	l.ly.mx = 4
	l.ly.my = 3

	fmt.Print(l.lx, l.ly.mx, l.ly.my, l.lz, "\n")
	fmt.Print(x.lx, x.ly.mx, x.ly.my, x.lz, "\n")
}

/*
== Expected program output ==
2 3 4 5
2 3 4 5
2 4 3 5
2 4 3 5
*/
