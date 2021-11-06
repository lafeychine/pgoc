package main

type A struct {
	b B
}

/*
== Expected compiler output ==
File "./tests/bad/struct/unknown_field_struct.go", line 4, characters 1-2:
error: undefined type of field b in structure A
*/
