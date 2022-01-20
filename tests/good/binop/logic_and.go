package main

import "fmt"

func id(a bool) bool {
	fmt.Print(a, "\n")
	return a
}

func main() {
	if id(false) && id(true) {
		fmt.Print("Bad\n")
	}

	if id(true) && id(false) {
		fmt.Print("Bad\n")
	}

	if id(true) && id(true) {
		fmt.Print("Hello\n")
	}
}

/*
== Expected program output ==
false
true
false
true
true
Hello
*/
