package main

// 481,54 -> 481,47 -> 481,54 -> 483,54 -> 483,45 -> 483,54 -> 485,54 -> 485,52 -> 485,54

import (
	"bufio"
	"fmt"
	"io"
	"math"
	"os"
	"strconv"
	"strings"
)

type Tuple = [2]int

type Chain = []Tuple

type Canvas struct {
	Canvas  map[Tuple]bool
	MinRows int
	MaxRows int
	MinCols int
	MaxCols int
}

func parseLines() ([]Chain, error) {
	items := make([]Chain, 0)

	f, err := os.Open("input")
	if err != nil {
		return items, err
	}

	defer f.Close()

	parse := func(str string) (Tuple, error) {

		parts := strings.Split(str, ",")
		one, err := strconv.ParseInt(strings.TrimSpace(parts[0]), 0, 32)
		if err != nil {
			return [2]int{}, err
		}
		two, err := strconv.ParseInt(strings.TrimSpace(parts[1]), 0, 32)
		if err != nil {
			return [2]int{}, err
		}

		return [2]int{int(one), int(two)}, nil
	}

	buf := bufio.NewReader(f)

	for {
		line, err := buf.ReadString('\n')
		if len(line) > 0 {
			pairsStr := strings.Split(line, " -> ")
			chainItems := make(Chain, 0)

			for _, pairStr := range pairsStr {
				pair, err := parse(pairStr)
				if err != nil {
					return items, err
				}

				chainItems = append(chainItems, pair)
			}

			items = append(items, chainItems)
		}
		if err == io.EOF {
			break
		}

		if err != nil {
			return items, err
		}
	}

	return items, nil
}

func createFilledOutCanvas(items []Chain) Canvas {
	canvas := make(map[Tuple]bool)

	mmin := Tuple{math.MaxInt, math.MaxInt}
	mmax := Tuple{math.MinInt, math.MinInt}

	for _, line := range items {
		for i := 0; i < len(line)-1; i++ {
			left := line[i]
			right := line[i+1]

			for x := 0; x <= 1; x++ {
				if left[x] < mmin[x] {
					mmin[x] = left[x]
				}
				if right[x] < mmin[x] {
					mmin[x] = right[x]
				}

				if left[x] > mmax[x] {
					mmax[x] = left[x]
				}
				if right[x] > mmax[x] {
					mmax[x] = right[x]
				}

			}

			chanding_idx := 0

			if left[0] == right[0] {
				chanding_idx = 1
			}

			start := left[chanding_idx]
			end := right[chanding_idx]

			if start > end {
				start = right[chanding_idx]
				end = left[chanding_idx]
			}

			for j := start; j <= end; j++ {
				point := left
				point[chanding_idx] = j

				canvas[point] = true
			}

		}
	}

	result := Canvas{
		Canvas:  canvas,
		MinRows: mmin[1],
		MaxRows: mmax[1],
		MinCols: mmin[0],
		MaxCols: mmax[0],
	}
	fmt.Printf("Cols: ( %v, %v ). Rows: (%v, %v)\n", result.MinCols, result.MaxCols, result.MinRows, result.MaxRows)
	return result
}

func printGrid(canvas Canvas) {
	grid := make(Chain, 0)

	for item := range canvas.Canvas {
		grid = append(grid, item)
	}

	for row := canvas.MinRows; row <= canvas.MaxRows; row++ {
		for col := canvas.MinCols; col <= canvas.MaxCols; col++ {
			it := Tuple{col, row}

			if canvas.Canvas[it] {
				fmt.Print("#")
			} else {
				fmt.Print(".")
			}
		}
		fmt.Println()
	}

}

func part1AndPart2(mapka Canvas) {
	start := Tuple{500, 0}
	current := start
	part2 := 0
	part1 := 0

	for {
		if part1 == 0 && current[1] > mapka.MaxRows {
			part1 = part2
		}
		next_row := Tuple{current[0], current[1] + 1}

		is_down_blocked, _ := mapka.Canvas[next_row]

		if !is_down_blocked && (next_row[1] < mapka.MaxRows+2) {
			current = next_row
		} else {
			left := Tuple{next_row[0] - 1, next_row[1]}

			is_left_blocked, _ := mapka.Canvas[left]
			if !is_left_blocked && (left[1] < mapka.MaxRows+2) {
				current = left
			} else {
				right := Tuple{next_row[0] + 1, next_row[1]}

				is_right_blocked, _ := mapka.Canvas[right]
				if !is_right_blocked && (next_row[1] < mapka.MaxRows+2) {
					current = right
				} else {
					part2 = part2 + 1
					mapka.Canvas[current] = true

					if current == start {
						break
					}

					current = start
				}
			}
		}

	}

	fmt.Printf("Part 1: %v \n", part1)
	fmt.Printf("Part 2: %v \n", part2)

}

func main() {
	items, err := parseLines()

	if err != nil {
		fmt.Println(err)
		return
	}

	canvas := createFilledOutCanvas(items)
	//printGrid(canvas)
	part1AndPart2(canvas)
	//printGrid(canvas)
}
