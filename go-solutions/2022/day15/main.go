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

	f, err := os.Open("input")
	//f, err := os.Open("test")
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

func findIntervals(rowY int32, sensors []Pair) []Point {
	intervals := make([]Point, 0)

	for _, pair := range sensors {
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

	fmt.Printf("Part 1: %v\n", sum)

	return merged
}

func part1(sensors []Pair) {
	intervals := findIntervals(2000000, sensors)
	mergeIntervals(intervals)
}

func part2(sensors []Pair) {
	all_down_lines := make(map[int32]bool, 0)
	all_up_lines := make(map[int32]bool, 0)

	for i := range sensors {
		//y = mx + b
		//45 down == -1 => y= -x + b => b = x + y
		//45 up == 1 => y= x + b => b = x - y (I know it's flipped but it simplifies the equations later on, both works)
		pair := sensors[i]

		d := pair.Radius + 1

		top_right_edge := pair.Sensor.x + pair.Sensor.y + d
		bottom_left_edge := pair.Sensor.x + pair.Sensor.y - d

		bottom_right_edge := pair.Sensor.x - pair.Sensor.y + d
		top_left_edge := pair.Sensor.x - pair.Sensor.y - d

		all_down_lines[top_right_edge] = true
		all_down_lines[bottom_left_edge] = true

		all_up_lines[bottom_right_edge] = true
		all_up_lines[top_left_edge] = true
	}

	down_lines := all_down_lines
	up_lines := all_up_lines

	// fmt.Println("down slopes")
	// fmt.Println(down_lines)
	// fmt.Println("down slopes END")
	// fmt.Println("up slopes")
	// fmt.Println(up_lines)
	// fmt.Println("up slopes END")

	beacons := make(map[Point]bool, 0)

	checkBoundaries := func(value int32) bool {
		//return value >= 0 && value <= 20
		return value >= 0 && value < 4_000_000
	}

	//calculate all possible beacon points as a result of intersection of all outer-boundary slopes
	for down := range down_lines {
		for up := range up_lines {
			x_slope := down + up

			//we should check it's even because it is Manhattan corrdinate system
			if x_slope%2 == 0 {
				x := x_slope / 2
				y := down - x

				if checkBoundaries(x) && checkBoundaries(y) {
					p := Point{
						x: x,
						y: y,
					}
					beacons[p] = true
				}
			}
		}
	}

	//find a beacon that is "far away" from _each_ sensor
	for b := range beacons {
		//fmt.Println(b)
		allPass := true
		for _, pair := range sensors {
			d := distance(b.x, b.y, pair.Sensor.x, pair.Sensor.y)

			if d <= pair.Radius {
				allPass = false
			}
		}

		if allPass {

			beacon_tuning_frequency := int64(b.x)*4_000_000 + int64(b.y)
			fmt.Printf("Found beacon: %v, Part2: %v\n", b, beacon_tuning_frequency)
		}
	}

}

func main() {
	fmt.Println("Day 15")
	sensors, err := parseLines()
	if err != nil {
		fmt.Println(err)
		return
	}

	part1(sensors)
	part2(sensors)
}
