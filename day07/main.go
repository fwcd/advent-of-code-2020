package main

import (
	"fmt"
	"io/ioutil"
	"regexp"
	"strconv"
)

type Edge struct {
	next   string
	weight int
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func dfs(node string, graph map[string][]Edge, visited *map[string]bool) {
	for _, edge := range graph[node] {
		next := edge.next
		_, ok := (*visited)[next]
		if !ok {
			(*visited)[next] = true
			dfs(next, graph, visited)
		}
	}
}

func addEdge(from string, to string, weight int, graph *map[string][]Edge) {
	_, ok := (*graph)[from]
	if !ok {
		(*graph)[from] = make([]Edge, 0)
	}
	(*graph)[from] = append((*graph)[from], Edge{next: to, weight: weight})
}

func main() {
	data, err := ioutil.ReadFile("resources/input.txt")
	check(err)

	input := string(data)

	rulePattern, err := regexp.Compile("([a-z ]+) bags contain ([^\\.]+)\\.")
	check(err)

	containedPattern, err := regexp.Compile("(\\d+) ([a-z ]+) bag")
	check(err)

	// Store graphs in both directions
	innerToOuter := make(map[string][]Edge)
	outerToInner := make(map[string][]Edge)

	rules := rulePattern.FindAllStringSubmatch(input, -1)

	for _, rule := range rules {
		outerBag := rule[1]
		for _, contained := range containedPattern.FindAllStringSubmatch(rule[2], -1) {
			count, err := strconv.Atoi(contained[1])
			check(err)
			innerBag := contained[2]
			addEdge(innerBag, outerBag, count, &innerToOuter)
			addEdge(outerBag, innerBag, count, &outerToInner)
		}
	}

	reachable := make(map[string]bool)
	dfs("shiny gold", innerToOuter, &reachable)
	fmt.Println("Part 1:", len(reachable))
}
