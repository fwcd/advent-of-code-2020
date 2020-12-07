package main

import (
	"fmt"
	"io/ioutil"
	"regexp"
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

	rulePattern, err := regexp.Compile("([a-z ]+) bags contain ([^\\.]+)\\.")
	check(err)

	containedPattern, err := regexp.Compile("(\\d+) ([a-z ]+) bag")
	check(err)

	graph := make(map[string][]string)
	rules := rulePattern.FindAllStringSubmatch(input, -1)

	for _, rule := range rules {
		outerBag := rule[1]
		for _, contained := range containedPattern.FindAllStringSubmatch(rule[2], -1) {
			innerBag := contained[1]
			_, exists := graph[innerBag]
			if !exists {
				graph[innerBag] = make([]string, 0)
			}
			graph[innerBag] = append(graph[innerBag], outerBag)
		}
	}

	fmt.Print(graph)
}
