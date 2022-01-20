package main

import "fmt"

func main() {
	if true {
		fmt.Print("Hello\n")
	}

	if false {
		fmt.Print("Bad\n")
	}
}

/*
== Expected program output ==
Hello
*/
