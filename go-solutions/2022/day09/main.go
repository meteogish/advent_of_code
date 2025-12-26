package main

import (
	"bytes"
	"fmt"
	"os"
	"strconv"
)

var steps = map[byte][2]int{
	byte('D'): {0, 1},
	byte('U'): {0, -1},
	byte('L'): {-1, 0},
	byte('R'): {1, 0},
}

type move struct {
	dir byte
	c   int
}

type knot [2]int

type rope []knot

func readInput(path string) (input []move, err error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}

	for line := range bytes.SplitSeq(data, []byte{'\n'}) {
		//fmt.Println(line[1:])
		c, err := strconv.Atoi(string(line[2:]))
		if err != nil {
			return nil, err
		}

		input = append(input, move{
			dir: line[0],
			c:   c,
		})
	}
	return input, nil
}

func main() {
	input, err := readInput("input")
	if err != nil {
		fmt.Println(err)
	}

	bothPartsAtOnce(input)
}

func (r *rope) movehead(step [2]int) {
	head := &(*r)[0]

	head[0] += step[0]
	head[1] += step[1]
	knots_len := len(*r)

	for i := 0; i < knots_len-1; i++ {
		adjust_pair(&(*r)[i], &(*r)[i+1])
	}
}

func adjust_pair(head, tail *knot) {
	colDiff := head[0] - tail[0]
	rowDiff := head[1] - tail[1]

	col := colDiff > 1 || colDiff < -1
	row := rowDiff > 1 || rowDiff < -1

	if col && row {
		tail[0] += colDiff / 2
		tail[1] += rowDiff / 2
	} else if row {
		tail[0] += colDiff
		tail[1] += rowDiff / 2
	} else if col {
		tail[0] += colDiff / 2
		tail[1] += rowDiff
	}
}

func bothPartsAtOnce(input []move) {
	rope_len := 10
	rope := make(rope, rope_len)

	tail := &rope[rope_len-1]

	visited := [2]map[knot]bool{
		//Part 1 - Second knot - right after the head
		{
			{0, 0}: true,
		},
		//Part 2 - Tail knot
		{
			{0, 0}: true,
		},
	}

	for _, move := range input {
		// fmt.Println("\nMove: ", move)

		step := steps[move.dir]
		for move.c > 0 {
			rope.movehead(step)
			visited[0][rope[1]] = true
			visited[1][*tail] = true

			move.c -= 1
		}

		// fmt.Println()
		// fmt.Println("Knots: ", rope)
		// fmt.Println()
		// draw(rope)
	}

	fmt.Println(len(visited[0]))
	fmt.Println(len(visited[1]))
}

func draw(knots rope) {
	for row := -20; row < 20; row++ {
		for col := -20; col < 20; col++ {
			p := [2]int{col, row}

			found := false
			for k := range knots {
				if knots[k] == p {
					found = true
					if k == 0 {
						fmt.Print("H")
					} else {
						fmt.Print(k)
					}
					break
				}
			}

			if !found {
				if p[0] == 0 && p[1] == 0 {
					fmt.Print("0")
				} else {
					fmt.Print("#")
				}
			}
		}

		fmt.Println()
	}

}
