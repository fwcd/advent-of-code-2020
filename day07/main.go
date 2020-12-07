package main

import (
	"fmt"
	"io/ioutil"
	"regexp"
	"strconv"
)

type edge struct {
	next   string
	weight int
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func dfsFrom(node string, graph map[string][]edge, visited *map[string]bool) {
	for _, edge := range graph[node] {
		next := edge.next
		_, ok := (*visited)[next]
		if !ok {
			(*visited)[next] = true
			dfsFrom(next, graph, visited)
		}
	}
}

func sumFrom(node string, graph map[string][]edge) int {
	result := 1
	for _, edge := range graph[node] {
		result += edge.weight * sumFrom(edge.next, graph)
	}
	return result
}

func addEdge(from string, to string, weight int, graph *map[string][]edge) {
	_, ok := (*graph)[from]
	if !ok {
		(*graph)[from] = make([]edge, 0)
	}
	(*graph)[from] = append((*graph)[from], edge{next: to, weight: weight})
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
	innerToOuter := make(map[string][]edge)
	outerToInner := make(map[string][]edge)

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
	dfsFrom("shiny gold", innerToOuter, &reachable)
	fmt.Println("Part 1:", len(reachable))

	sum := sumFrom("shiny gold", outerToInner)
	fmt.Println("Part 2:", sum-1)
}
