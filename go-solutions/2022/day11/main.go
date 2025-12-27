package main

import (
	"bytes"
	"fmt"
	"os"
	"slices"
	"strconv"
	"strings"
)

type monkey struct {
	starting_items []int
	operation      func(int) int
	test           [3]int // { divisible by, if true monkey, if false monkey }
}

func parseInput(path string) (monkeys []monkey, err error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}

	sections := bytes.Split(data, []byte{'\n', '\n'})

	for _, section := range sections {
		lines := bytes.Split(section, []byte{'\n'})

		//Monkey 1:
		// Starting items: 76, 97, 58, 72, 57, 92, 82
		// Operation: new = old + 4
		// Test: divisible by 3
		//   If true: throw to monkey 7
		//   If false: throw to monkey 5

		// Starting items: 76, 97, 58, 72, 57, 92, 82
		staring_items_line := lines[1]
		items_start := bytes.IndexByte(staring_items_line, ':') + 2
		items := bytes.Split(staring_items_line[items_start:], []byte{','})
		starting_items := make([]int, len(items))
		for i, itm := range items {
			c, err := strconv.Atoi(strings.TrimSpace(string(itm)))
			if err != nil {
				return nil, err
			}
			starting_items[i] = c
		}

		// Operation: new = old + 4
		operation_line := lines[2]
		operation_start := bytes.IndexByte(operation_line, '=') + 6
		opByte := operation_line[operation_start]
		operandStr := string(bytes.TrimSpace(operation_line[operation_start+1:]))

		var operation func(int) int

		fmt.Println("Operand: ", operandStr)
		if operandStr == "old" {
			switch opByte {
			case '*':
				operation = func(x int) int {
					return x * x
				}
			case '+':
				operation = func(x int) int {
					return x + x
				}
			default:
				operation = nil
			}
		} else {
			operand, err := strconv.Atoi(operandStr)
			if err != nil {
				return nil, err
			}

			switch opByte {
			case '*':
				operation = func(x int) int {
					return x * operand
				}
			case '+':
				operation = func(x int) int {
					return x + operand
				}
			default:
				operation = nil
			}
		}

		// Test: divisible by 3
		//   If true: throw to monkey 7
		//   If false: throw to monkey 5
		div_by_line := lines[3][bytes.LastIndexByte(lines[3], ' '):]
		true_line := lines[4][bytes.LastIndexByte(lines[4], ' '):]
		false_line := lines[5][bytes.LastIndexByte(lines[5], ' '):]

		test := [3]int{}

		for i, x := range [][]byte{div_by_line, true_line, false_line} {
			val, err := strconv.Atoi(string(bytes.TrimSpace(x)))
			if err != nil {
				return nil, err
			}

			test[i] = val
		}

		monkeys = append(monkeys, monkey{
			starting_items,
			operation,
			test,
		})
	}

	return monkeys, nil
}

func main() {
	input, err := parseInput("input")
	if err != nil {
		fmt.Println(err)
	}

	fmt.Println(input)

	part1(input, 20, func(x int) int { return x / 3 })
}

func part1(input []monkey, rounds int, conv func(int) int) {

	activity := make([]int, len(input))

	for round := range rounds {
		//fmt.Println("Round ", round+1)

		for i := range len(input) {

			curr := &input[i]

			for _, old := range curr.starting_items {
				activity[i] += 1
				//fmt.Println("Monkey inspects an item with a worry level of ", old)
				new := curr.operation(old)
				//fmt.Println("Worry level is now: ", new)
				new = conv(new)
				new = new % bigMod
				//fmt.Println("Monkey gets bored with item. Worry level is divided by 3 to ", new)

				var next_monkey *monkey

				if new%curr.test[0] == 0 {
					//fmt.Println("TEST IS TRUE: divisible by ", curr.test[0])
					next_monkey = &input[curr.test[1]]
					//fmt.Printf("Item with worry level %d is thrown to monkey %d\n", new, curr.test[1])
				} else {
					//fmt.Println("TEST IS FALSE: divisible by ", curr.test[0])
					next_monkey = &input[curr.test[2]]
					//fmt.Printf("Item with worry level %d is thrown to monkey %d\n", new, curr.test[2])
				}

				next_monkey.starting_items = append(next_monkey.starting_items, new)

				//fmt.Println()
			}
			curr.starting_items = []int{}
			fmt.Println()
		}
		fmt.Printf("After Round %d \n", round+1)
		for i := range len(input) {
			curr := &input[i]

			fmt.Printf("Monkey %d: %v\n", i, curr.starting_items)
		}
	}

	slices.Sort(activity)

	two := activity[len(activity)-2:]

	fmt.Println(two[0] * two[1])
}
