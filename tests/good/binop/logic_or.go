package main

import "fmt"

func id(a bool) bool {
	fmt.Print(a, "\n")
	return a
}

func main() {
	if id(false) || id(true) {
		fmt.Print("Hello\n")
	}

	if id(true) || id(false) {
		fmt.Print("Hello\n")
	}
}

/*
== Expected program output ==
false
true
Hello
true
Hello
*/
