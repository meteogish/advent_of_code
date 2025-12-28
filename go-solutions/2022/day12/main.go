package main

import (
	"bytes"
	"fmt"
	"os"
	"slices"
)

// { row, col }
type position [2]int

type hill struct {
	data  [][]byte
	start position
	end   position
}

type queueItem struct {
	pos  position
	dist int
}

type isNeighbour func(byte, byte, position) bool

var steps = []position{
	{0, 1},
	{0, -1},
	{-1, 0},
	{1, 0},
}

func readInput(path string) (hill, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return hill{}, err
	}
	lines := bytes.Split(data, []byte{'\n'})

	var start [2]int
	var end [2]int
	for row, line := range lines {
		for col, c := range line {
			if c == 'S' {
				start[0] = row
				start[1] = col
			}
			if c == 'E' {
				end[0] = row
				end[1] = col
			}
		}
	}
	return hill{
		data:  lines,
		start: start,
		end:   end,
	}, nil
}

func (h *hill) val(pos position) byte {
	return h.data[pos[0]][pos[1]]
}

func main() {
	input, err := readInput("input")
	//input, err := readInput("test")
	if err != nil {
		fmt.Println(err)
	}

	fmt.Printf("Part 1: %v\n", part1(input))
	fmt.Printf("Part 2: %v\n", part2(input))
}

func part1(input hill) int {
	isNeighbour := func(val, nextVal byte, pos position) bool {
		return val == 'S' || (val == 'z' && nextVal == 'E') || (nextVal != 'E' && val+1 >= nextVal)
	}

	return countMinDistance(input, func(val byte) bool { return val == 'E' }, isNeighbour)
}

func part2(input hill) int {
	isNeighbour := func(val, nextVal byte, pos position) bool {
		return val == 'a' || (val == 'E' && nextVal == 'z') || (val != 'E' && nextVal+1 >= val)
	}

	// Start from the top of the hill 'E' and move to the 'a' capturing the shortest path
	input = hill{
		data:  input.data,
		start: input.end,
		end:   input.start,
	}

	return countMinDistance(input, func(val byte) bool { return val == 'a' || val == 'S' }, isNeighbour)
}

func countMinDistance(input hill, isTarget func(byte) bool, isNeighbour isNeighbour) int {
	queue := []queueItem{{input.start, 0}}
	set := map[position]bool{}
	set[input.start] = true

	for len(queue) > 0 {
		curr := queue[0]
		//fmt.Printf("Curr: %v, val: %v\n", curr.pos, string(input.val(curr.pos)))
		if isTarget(input.val(curr.pos)) {
			//fmt.Printf("Found: %v \n", curr)
			return curr.dist
		}
		neigh := getNeighbours(input, curr, func(val byte, nextVal byte, nextPos position) bool {
			if isNeighbour(val, nextVal, nextPos) {
				_, ok := set[nextPos]
				if !ok {
					set[nextPos] = true
				}
				return !ok
			}
			return false
		})

		// neigh = slices.DeleteFunc(neigh, func(item queueItem) bool {
		// 	_, ok := set[item.pos]
		// 	if !ok {
		// 		set[item.pos] = true
		// 	}
		// 	return ok
		// })

		queue = slices.Concat(queue[1:], neigh)

		//fmt.Println(queue)
		//fmt.Println()
	}

	return -1
}

func getNeighbours(input hill, curr queueItem, isNeighbour isNeighbour) []queueItem {
	pos := curr.pos
	val := input.val(pos)
	neighbours := slices.Collect(func(yield func(queueItem) bool) {
		for _, n := range steps {
			nextPos := position{pos[0] + n[0], pos[1] + n[1]}
			item := queueItem{nextPos, curr.dist + 1}
			if nextPos[0] >= 0 && nextPos[0] < len(input.data) && nextPos[1] >= 0 && nextPos[1] < len(input.data[0]) {
				nextVal := input.val(nextPos)
				if isNeighbour(val, nextVal, nextPos) {
					if !yield(item) {
						return
					}
				}
			}
		}
	})

	return neighbours
}
