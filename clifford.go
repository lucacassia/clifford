package main

import (
	"errors"
	"fmt"
	"os"
	"strconv"
	"strings"
)

const (
	R = "ℝ"
	C = "ℂ"
	H = "ℍ"
)

type Element struct {
	algebra string
	matrix  int
	copies  int
}

func (e Element) String() string {
	k := fmt.Sprintf("%s(%d)", e.algebra, e.matrix)
	return strings.Join(strings.Fields(strings.Repeat(k+" ", e.copies)), "⊕")
}

var memo = map[[2]int]Element{}

func otimes(a, b Element) (Element, error) {
	switch a.algebra {
	case R:
		return Element{b.algebra, a.matrix * b.matrix, a.copies * b.copies}, nil
	case C:
		switch b.algebra {
		case R:
			return Element{C, a.matrix * b.matrix, a.copies * b.copies}, nil
		case C:
			return Element{C, a.matrix * b.matrix, 2 * a.copies * b.copies}, nil
		case H:
			return Element{C, 2 * a.matrix * b.matrix, a.copies * b.copies}, nil
		}
	case H:
		switch b.algebra {
		case R:
			return Element{H, a.matrix * b.matrix, a.copies * b.copies}, nil
		case C:
			return Element{C, 2 * a.matrix * b.matrix, a.copies * b.copies}, nil
		case H:
			return Element{R, 4 * a.matrix * b.matrix, a.copies * b.copies}, nil
		}
	}
	return Element{}, errors.New("invalid algebra type in otimes")
}

func clifford(s, t int) (Element, error) {
	if s < 0 || t < 0 {
		return Element{}, errors.New("s and t must be non-negative")
	}

	key := [2]int{s, t}
	if cached, ok := memo[key]; ok {
		return cached, nil
	}

	var (
		result Element
		err    error
	)

	switch {
	case s == 0 && t == 0:
		result = Element{R, 1, 1}
	case s == 0 && t == 1:
		result = Element{R, 1, 2}
	case s == 1 && t == 0:
		result = Element{C, 1, 1}
	case s > 0 && t > 0:
		inner, err := clifford(s-1, t-1)
		if err != nil {
			return Element{}, err
		}
		result, err = otimes(Element{R, 2, 1}, inner)
	case s == 0 && t > 1:
		inner, err := clifford(t-2, s)
		if err != nil {
			return Element{}, err
		}
		result, err = otimes(Element{R, 2, 1}, inner)
	case s > 1 && t == 0:
		inner, err := clifford(t, s-2)
		if err != nil {
			return Element{}, err
		}
		result, err = otimes(Element{H, 1, 1}, inner)
	}

	if err != nil {
		return Element{}, err
	}

	memo[key] = result
	return result, nil
}

func main() {
	if len(os.Args) < 3 {
		fmt.Fprintf(os.Stderr, "Usage: %s <s> <t>\n", os.Args[0])
		os.Exit(1)
	}

	s, err := strconv.Atoi(os.Args[1])
	if err != nil {
		fmt.Fprintf(os.Stderr, "Invalid argument for s: %v\n", err)
		os.Exit(1)
	}

	t, err := strconv.Atoi(os.Args[2])
	if err != nil {
		fmt.Fprintf(os.Stderr, "Invalid argument for t: %v\n", err)
		os.Exit(1)
	}

	cl, err := clifford(s, t)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Cl(%s,%s) = %s\n", os.Args[1], os.Args[2], cl)
}
