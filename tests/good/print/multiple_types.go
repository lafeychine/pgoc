package main

import "fmt"

func main() {
	fmt.Print(1, 2, "Hello", "World", nil, 3, "!", 4, "\n")
}

/*
== Expected program output ==
1 2HelloWorld<nil> 3!4
*/
