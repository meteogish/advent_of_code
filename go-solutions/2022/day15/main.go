package main

import (
	"bufio"
	"fmt"
	"io"
	"math"
	"os"
	"regexp"
	"slices"
	"strconv"
	"strings"
)

type Point struct {
	x, y int32
}

type Pair struct {
	Sensor Point
	Beacon Point
	Radius int32
}

func parseLine(line string) (Pair, error) {
	rg := regexp.MustCompile(`Sensor at x=(-?\d*), y=(-?\d*): closest beacon is at x=(-?\d*), y=(-?\d*)`)

	match := rg.FindStringSubmatch(line)
	// fmt.Println(len(match))
	// fmt.Println(match)

	parsedMatch := make([]int32, len(match))

	for i := range len(match) - 1 {
		val, err := strconv.ParseInt(strings.TrimSpace(match[i+1]), 0, 32)
		if err != nil {
			return Pair{}, err
		}

		parsedMatch[i] = int32(val)
	}
	sensor := Point{parsedMatch[0], parsedMatch[1]}
	beacon := Point{parsedMatch[2], parsedMatch[3]}

	return Pair{
		Sensor: sensor,
		Beacon: beacon,
		Radius: distance(sensor.x, sensor.y, beacon.x, beacon.y),
	}, nil

}

func parseLines() ([]Pair, error) {
	items := make([]Pair, 0)

	//f, err := os.Open("input")
	f, err := os.Open("test")
	if err != nil {
		return items, err
	}

	defer f.Close()

	buf := bufio.NewReader(f)

	for {
		line, err := buf.ReadString('\n')
		if len(line) > 0 {
			item, err := parseLine(line)
			if err != nil {
				return items, err
			}

			items = append(items, item)
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

func distance(x1, y1, x2, y2 int32) int32 {
	return int32(math.Abs(float64(x1)-float64(x2)) + math.Abs(float64(y1)-float64(y2)))
}

func absDiff(a, b int32) int32 {
	return int32(math.Abs(float64(a) - float64(b)))
}

func findIntervals(rowY int32, allPairs []Pair) []Point {
	intervals := make([]Point, 0)

	for _, pair := range allPairs {
		dist := absDiff(rowY, pair.Sensor.y)

		if dist <= pair.Radius {
			rest := pair.Radius - dist
			intervals = append(intervals, Point{
				x: pair.Sensor.x - rest, //start
				y: pair.Sensor.x + rest, //end
			})
		}

	}

	slices.SortFunc(intervals, func(a, b Point) int {
		return int(a.x - b.x)
	})

	return intervals
}

func mergeIntervals(intervals []Point) []Point {
	slices.SortFunc(intervals, func(a, b Point) int {
		return int(a.x - b.x)
	})

	merged := make([]Point, 0)

	for i := 0; i < len(intervals)-1; i++ {
		left_start := intervals[i].x
		left_end := intervals[i].y

		right_start := intervals[i+1].x
		right_end := intervals[i+1].y

		if right_start <= left_end {

			p := Point{
				x: left_start,
				y: max(right_end, left_end),
			}

			//fmt.Printf("puting %v into intervals at idx %v\n", p, i+1)
			intervals[i+1] = p
		} else {
			merged = append(merged, intervals[i])
		}

		// fmt.Println("####")
		// fmt.Println(intervals)
		// fmt.Println("####")
	}

	merged = append(merged, intervals[len(intervals)-1])

	sum := int32(0)

	for _, p := range merged {
		sum += int32(math.Abs(float64(p.x))) + int32(math.Abs(float64(p.y)))
	}

	fmt.Println(sum)

	return merged
}

func main() {
	fmt.Println("Day 15")
	pairs, err := parseLines()
	if err != nil {
		fmt.Println(err)
		return
	}

	filterPart2 := make([]Pair, 0)

	for _, p := range pairs {
		if p.Beacon.x >= 0 && p.Beacon.x < 20 && p.Beacon.y >= 0 && p.Beacon.y < 20 {
			filterPart2 = append(filterPart2, p)
		}
	}

	//intervals := findIntervals(2000000, pairs)
	intervals := findIntervals(10, filterPart2)
	fmt.Println(intervals)
	fmt.Println()
	fmt.Println(mergeIntervals(intervals))
}
