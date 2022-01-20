package main

import "fmt"

func main() {
	x := 42
	{
		x := "Hello"
		fmt.Print(x, "\n")
	}
	fmt.Print(x, "\n")
}

/*
== Expected program output ==
Hello
42
*/
