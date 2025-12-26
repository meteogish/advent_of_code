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

	fmt.Println("Part 1: ", part0(input, 10))
}

func part1(input []move) int {

	var head, tail [2]int

	visited := map[[2]int]bool{
		{0, 0}: true,
	}

	for _, move := range input {

		step := steps[move.dir]
		for move.c > 0 {
			head[0] += step[0]
			head[1] += step[1]

			colDiff := head[0] - tail[0]
			rowDiff := head[1] - tail[1]

			if colDiff > 1 || colDiff < -1 {
				tail[0] += colDiff / 2
				if rowDiff != 0 {
					tail[1] += rowDiff
				}
			}

			if rowDiff > 1 || rowDiff < -1 {
				tail[1] += rowDiff / 2

				if colDiff != 0 {
					tail[0] += colDiff
				}
			}

			visited[tail] = true

			move.c -= 1
		}
	}

	return len(visited)
}

func recalc(head, tail *[2]int) {
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

func part0(input []move, knots_len int) int {
	knots := make([][2]int, knots_len)

	head := &knots[0]
	tail := &knots[knots_len-1]

	visited := map[[2]int]bool{
		{0, 0}: true,
	}

	for _, move := range input {
		// fmt.Println()
		// fmt.Println("Move: ", move)
		// fmt.Println()

		step := steps[move.dir]
		for move.c > 0 {
			head[0] += step[0]
			head[1] += step[1]

			for i := 0; i < knots_len-1; i++ {
				recalc(&knots[i], &knots[i+1])
				//fmt.Println("recalc knot ", i, "State: ", knots)
			}

			visited[*tail] = true

			move.c -= 1
		}

		// fmt.Println()
		// fmt.Println("Knots: ", knots)
		// fmt.Println()
		// draw(knots)
	}

	return len(visited)
}

func draw(knots [][2]int) {
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
