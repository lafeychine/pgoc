package main

import "fmt"

type L struct {
	lx int
	ly *M
}

type M struct {
	mx int
	my *N
	mz string
}

type N struct {
	nx int
	ny string
}

func main() {
	l := new(L)
	l.ly = new(M)
	l.ly.my = new(N)

	l.lx = 2
	l.ly.mx = 4
	l.ly.my.nx = 5
	l.ly.my.ny = "Hello"
	l.ly.mz = "World"

	fmt.Print(l.lx, l.ly.mx, l.ly.my.nx, l.ly.my.ny, l.ly.mz, "\n")
}

/*
== Expected program output ==
2 4 5HelloWorld
*/
