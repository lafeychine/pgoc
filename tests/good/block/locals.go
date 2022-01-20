package main

import "fmt"

func main() {
	x := 42
	{
		y := 84
		fmt.Print(x, y, "\n")
	}
	fmt.Print(x, "\n")
}

/*
== Expected program output ==
42 84
42
*/
