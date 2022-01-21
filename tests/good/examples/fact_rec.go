package main

import "fmt"

func fact(n int) int {
	if n <= 1 {
		return 1
	}
	return n * fact(n-1)
}

func main() {
	for n := 0; n <= 10; n++ {
		fmt.Print(fact(n), "\n")
	}
}

/*
== Expected program output ==
1
1
2
6
24
120
720
5040
40320
362880
3628800
*/
