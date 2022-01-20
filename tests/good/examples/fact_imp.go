package main

import "fmt"

func fact(n int) int {
	r := 1
	for ; n > 1; n-- {
		r = r * n
	}
	return r
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
