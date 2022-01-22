package main

import "fmt"

func main() {
	fmt.Print(main())
}

/*
== Expected compiler output ==
File "./tests/bad/fmt/void_fmt.go", line 6, characters 11-17:
error: cannot print void type
*/
