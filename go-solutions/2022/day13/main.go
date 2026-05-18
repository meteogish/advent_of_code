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

func readFile() (int, error) {
	file, err := os.Open("./input")
	if err != nil {
		return 0, err
	}

	defer file.Close()

	pairIdx := 1
	sum := 0

	i := 0

	pair := [2]string{}

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()

		if len(line) == 0 {
			if cmp(&pair[0], &pair[1]) == -1 {
				fmt.Println("pairs same: ", pairIdx)
				sum += pairIdx
			}
			pairIdx += 1
		} else {
			pair[i] = line
			i = (i + 1) % 2
		}
	}
	if cmp(&pair[0], &pair[1]) == -1 {
		sum += pairIdx
	}

	return sum, nil
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

	// if i < (lenl-1) && string([]byte{(*l)[i], (*l)[i+1]}) == "[]" {
	// 	return TokenInfo{Kind: EmptyList, EndIdx: i + 2}
	// }
	// if (*l)[i] == ',' {
	// 	return advanceToken(l, lenl, i+1)
	// }

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

func cmpLiteralCons(literal int64, l *string, idx_s *int) int {
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

	//const other_literal = std.fmt.parseInt(u8, l[lit_start:idx], 10) catch unreachable;
	other_literal, err0 := strconv.ParseInt((*l)[lit_start:idx], 10, 8)

	if err0 != nil {
		return -1
	}

	if literal < other_literal {
		return -1
	}
	if literal > other_literal {
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

			//t1 is literal or cons
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
				//TODO: compareLiteralWithCons
				res := cmpLiteralCons(lit0, l1, &i1)
				if res != 0 {
					return res
				}
				// match
				i0 = t0.EndIdx
				continue
			}
		}

		if t0.Kind == List {
			// cons vs nil
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
			res := cmpLiteralCons(lit1, l0, &i0)
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

func main() {
	res, err := readFile()
	if err != nil {
		fmt.Println("{v}", err)
	} else {
		fmt.Println("{v}", res)
	}
}
