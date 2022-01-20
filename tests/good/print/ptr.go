package main

import "fmt"

func main() {
	var x *int

	y := &x

	fmt.Print(&y, &y, nil, &y)
}

/*
 */
