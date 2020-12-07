package main

import (
	"fmt"
	"io/ioutil"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	data, err := ioutil.ReadFile("resources/input.txt")
	check(err)

	input := string(data)
	fmt.Println(input)
}
