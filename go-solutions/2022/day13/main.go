package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

type node struct {
	val   []int
	nodes []node
}

const file = "./input"

func main() {
	result, err := bothPartsInO_N()
	if err != nil {
		fmt.Println(err)
	} else {
		fmt.Println("Part 1&2: ", result)
	}
}

func bothPartsInO_N() ([]int, error) {
	file, err := os.Open(file)
	if err != nil {
		return nil, err
	}

	defer file.Close()

	//for part1
	pairIdx := 1
	part1 := 0
	i := 0
	pair := [2]string{}

	//for part2
	el2 := "[[2]]"
	el6 := "[[6]]"
	//How many elems before el2 and el6 would appear
	beforeEl2 := 0
	beforeEl6 := 0

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()

		if len(line) == 0 {
			if cmp(&pair[0], &pair[1]) == -1 {
				fmt.Println("pairs same: ", pairIdx)
				part1 += pairIdx
			}
			pairIdx += 1
		} else {
			pair[i] = line
			i = (i + 1) % 2

			//for part2
			if cmp(&line, &el2) < 0 {
				beforeEl2 += 1
			}

			if cmp(&line, &el6) < 0 {
				beforeEl6 += 1
			}
		}
	}
	if cmp(&pair[0], &pair[1]) == -1 {
		part1 += pairIdx
	}

	//+2 because
	// el2 will occupy 1 place and shift
	// and then el6 will occupy 1 place and shift
	// and the numeration is naturally from 1
	part2 := (beforeEl2 + 1) * (beforeEl6 + 2)

	return []int{part1, part2}, nil
}

const (
	Nil int = iota
	List
	Number
)

type TokenInfo struct {
	Kind   int
	EndIdx int
}

func advanceToken(l *string, lenl int, i int) TokenInfo {
	if (*l)[i] == ']' || (*l)[i] == ',' {
		return TokenInfo{Kind: Nil, EndIdx: i + 1}
	}

	if (*l)[i] == '[' {
		if i < (lenl-1) && (*l)[i+1] == ']' {
			return TokenInfo{Kind: Nil, EndIdx: i + 2}
		}

		return TokenInfo{Kind: List, EndIdx: i + 1}
	}

	s := i + 1

	for s < lenl && (*l)[s] != ',' && (*l)[s] != ']' {
		s += 1
	}

	return TokenInfo{Kind: Number, EndIdx: s}
}

func compareListAndNumber(number int64, l *string, idx_s *int) int {
	depth := 0
	idx := *idx_s
	for (*l)[idx] == '[' {
		idx += 1
		depth += 1
	}

	// l[idx] is now a literal or closing brace
	lit_start := idx

	// literal vs nil
	if (*l)[lit_start] == ']' {
		return 1
	}

	for (*l)[idx] != ',' && (*l)[idx] != ']' {
		idx += 1
	}

	//const other_number = std.fmt.parseInt(u8, l[lit_start:idx], 10) catch unreachable;
	other_number, err0 := strconv.ParseInt((*l)[lit_start:idx], 10, 8)

	if err0 != nil {
		return -1
	}

	if number < other_number {
		return -1
	}
	if number > other_number {
		return 1
	}

	// literals match, try to back out by depth
	for (*l)[idx] == ']' && depth > 0 {
		idx += 1
		depth -= 1
	}

	// didn't back out all the way
	if depth > 0 {
		return -1
	}

	// backed out successfully
	// depth == 0
	*idx_s = idx
	return 0
}

func cmp(l0 *string, l1 *string) int {
	i0 := 0
	i1 := 0

	len0 := len(*l0)
	len1 := len(*l1)

	for i0 < len0 && i1 < len1 {
		t0 := advanceToken(l0, len0, i0)
		t1 := advanceToken(l1, len1, i1)

		if t0.Kind == Nil {
			if t1.Kind == Nil {
				i0 = t0.EndIdx
				i1 = t1.EndIdx
				continue
			}

			//t1 is number or a non-empty list
			return -1
		}

		if t0.Kind == Number {
			lit0, err0 := strconv.ParseInt((*l0)[i0:t0.EndIdx], 10, 8)

			if err0 != nil {
				return -1
			}

			if t1.Kind == Nil {
				return 1
			}

			if t1.Kind == Number {
				lit1, err1 := strconv.ParseInt((*l1)[i1:t1.EndIdx], 10, 8)

				if err1 != nil {
					return -1
				}

				if lit0 == lit1 {
					i0 = t0.EndIdx
					i1 = t1.EndIdx
					continue
				}

				if lit0 < lit1 {
					return -1
				}
				return 1
			}

			if t1.Kind == List {
				res := compareListAndNumber(lit0, l1, &i1)
				if res != 0 {
					return res
				}
				// match
				i0 = t0.EndIdx
				continue
			}
		}

		if t0.Kind == List {
			if t1.Kind == Nil {
				return 1
			}

			// cons vs cons
			if t1.Kind == List {
				i0 = t0.EndIdx
				i1 = t1.EndIdx
				continue
			}

			lit1, err := strconv.ParseInt((*l1)[i1:t1.EndIdx], 10, 8)
			if err != nil {
				return -1
			}
			res := compareListAndNumber(lit1, l0, &i0)
			if res != 0 {
				return res * -1
			}
			// match
			i1 = t1.EndIdx
			continue
		}
	}

	if i1 == len1 {
		return 0
	}

	return -1
}
