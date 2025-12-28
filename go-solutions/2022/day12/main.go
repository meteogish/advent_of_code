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

	fmt.Println(input.start)
	fmt.Println(input.end)
	part1(input)
}

type queueItem struct {
	pos  position
	dist int
}

func part1(input hill) {
	queue := []queueItem{{input.start, 0}}
	set := map[position]bool{}
	set[input.start] = true

	for len(queue) > 0 {
		curr := queue[0]
		fmt.Printf("Curr: %v, val: %v\n", curr.pos, string(input.val(curr.pos)))
		if input.val(curr.pos) == 'E' {
			fmt.Printf("Found: %v \n", curr)
			break
		}
		neigh := getNeighbours(input, curr)

		neigh = slices.DeleteFunc(neigh, func(item queueItem) bool {
			_, ok := set[item.pos]
			if !ok {
				set[item.pos] = true
			}
			return ok
		})

		queue = slices.Concat(queue[1:], neigh)

		fmt.Println(queue)
		fmt.Println()
	}
}

func getNeighbours(input hill, curr queueItem) []queueItem {
	pos := curr.pos
	var steps = []position{
		position{0, 1},
		position{0, -1},
		position{-1, 0},
		position{1, 0},
	}

	val := input.val(pos)
	neighbours := slices.Collect(func(yield func(queueItem) bool) {
		for _, n := range steps {
			next := position{pos[0] + n[0], pos[1] + n[1]}
			item := queueItem{next, curr.dist + 1}
			if next[0] >= 0 && next[0] < len(input.data) && next[1] >= 0 && next[1] < len(input.data[0]) {
				nextVal := input.val(next)
				if val == 'S' || (val == 'z' && nextVal == 'E') || (nextVal != 'E' && val+1 >= input.val(next)) {
					if nextVal == 'E' {
						fmt.Printf("Adding E: %v, %v\n", string(val), pos)
						fmt.Printf("%v\n", val == 'z')
					}
					if !yield(item) {
						return
					}
				}
			}
		}
	})

	// slices.SortFunc(neighbours, func(a, b position) int {
	// // get a distance between the point and a target
	// // sort by lesser distance
	// 	return 1
	// })
	return neighbours
}
