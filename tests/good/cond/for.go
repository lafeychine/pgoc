package main

import "fmt"

func main() {
	for countdown := 10; countdown > 0; countdown-- {
		fmt.Print(countdown, "\n")
	}

	fmt.Print("Liftoff\n")
}

/*
== Expected program output ==
10
9
8
7
6
5
4
3
2
1
Liftoff
*/
