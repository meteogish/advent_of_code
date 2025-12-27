package main

import (
	"bytes"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type instruction struct {
	cycles int
	value  int
}

func readInput(path string) (input []instruction, err error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}

	addx := []byte("addx")
	noop := []byte("noop")

	for line := range bytes.SplitSeq(data, []byte{'\n'}) {
		//fmt.Println(line[1:])
		var i instruction
		if bytes.Equal(line[:4], addx) {
			c, err := strconv.Atoi(strings.TrimSpace(string(line[4:])))
			if err != nil {
				return nil, err
			}
			i = instruction{
				cycles: 2,
				value:  c,
			}
		} else if bytes.Equal(line[:4], noop) {
			i = instruction{
				cycles: 1,
				value:  0,
			}
		}

		input = append(input, i)
	}
	return input, nil
}

func main() {
	input, err := readInput("input")
	if err != nil {
		fmt.Println(err)
	}

	part1(input)
	part2(input)
}

func part1(input []instruction) {
	r := 1
	cycles := 0
	next_cycle := 20

	result := 0

	for _, op := range input {
		for op.cycles > 0 {
			cycles += 1

			if cycles == next_cycle {
				result += next_cycle * r
				next_cycle += 40
			}

			op.cycles -= 1
		}

		r += op.value
	}

	for next_cycle < 220 {
		result += next_cycle * r
		next_cycle += 40
	}

	fmt.Println(result)
}

func part2(input []instruction) {
	r := 1
	cycles := 0

	bob := strings.Builder{}

	for _, op := range input {
		for op.cycles > 0 {
			if r-1 == cycles || r+1 == cycles || r == cycles {
				bob.WriteByte('#')
			} else {
				bob.WriteByte('.')
			}

			cycles += 1
			op.cycles -= 1

			if cycles == 40 {
				bob.WriteByte('\n')
				cycles = 0
			}
		}

		r += op.value
	}

	fmt.Printf("%s", bob.String())
}
