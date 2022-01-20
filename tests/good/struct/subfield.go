package main

import "fmt"

type L struct {
	lx int
	ly M
}

type M struct {
	mx int
	my N
	mz string
}

type N struct {
	nx int
	ny string
}

func main() {
	var l L

	l.lx = 2
	l.ly.mx = 4
	l.ly.my.nx = 5
	l.ly.my.ny = "Hello"
	l.ly.mz = "World"

	fmt.Print(l.lx, l.ly.mx, l.ly.my.nx, l.ly.my.ny, l.ly.mz, "\n")
	fmt.Print(l.lx, l.ly.mx, l.ly.my, l.ly.mz, "\n")
	fmt.Print(l.lx, l.ly, "\n");
	fmt.Print(l, "\n")
}

/*
== Expected program output ==
2 4 5HelloWorld
2 4 {5 Hello}World
2 {4 {5 Hello} World}
{2 {4 {5 Hello} World}}
*/
