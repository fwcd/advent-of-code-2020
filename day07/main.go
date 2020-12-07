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

func dfs(inner string, graph map[string][]string, visited *map[string]bool) {
	for _, outer := range graph[inner] {
		_, ok := (*visited)[outer]
		fmt.Println("Outer", outer, ok)
		if !ok {
			(*visited)[outer] = true
			dfs(outer, graph, visited)
		}
	}
}

func main() {
	data, err := ioutil.ReadFile("resources/input.txt")
	check(err)

	input := string(data)

	rulePattern, err := regexp.Compile("([a-z ]+) bags contain ([^\\.]+)\\.")
	check(err)

	containedPattern, err := regexp.Compile("(\\d+) ([a-z ]+) bag")
	check(err)

	graph := make(map[string][]string)
	rules := rulePattern.FindAllStringSubmatch(input, -1)

	for _, rule := range rules {
		outerBag := rule[1]
		for _, contained := range containedPattern.FindAllStringSubmatch(rule[2], -1) {
			innerBag := contained[2]
			_, ok := graph[innerBag]
			if !ok {
				graph[innerBag] = make([]string, 0)
			}
			graph[innerBag] = append(graph[innerBag], outerBag)
		}
	}

	fmt.Println(graph)
	reachable := make(map[string]bool)
	dfs("shiny gold", graph, &reachable)
	fmt.Println("Part 1:", len(reachable))
}
