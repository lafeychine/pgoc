package main

import "fmt"

func main() {
	fmt.Print(42 < 84, "\n")
	fmt.Print(42 > 84, "\n")
	fmt.Print(42 >= 84, "\n")
	fmt.Print(42 >= 42, "\n")
	fmt.Print(42 <= 84, "\n")
	fmt.Print(42 <= 42, "\n")
}

/*
== Expected program output ==
true
false
false
true
true
true
*/
