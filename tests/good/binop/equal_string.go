package main

import "fmt"

func main() {
	s1 := "Hello"
	s2 := "Hello"
	s3 := "Hello World"

	if (s1 == s2) {
		fmt.Print("s1 == s2\n");
	}
	if (s1 == s3) {
		fmt.Print("s1 == s3\n")
	}
}

/*
== Expected program output ==
s1 == s2
*/
