package core_test

import (
	"testing"

	"../core"
)

func TestRotate(t *testing.T) {
	x := []int{1, 5, 2, 4, 2}
	y := []int{0, 5, 0, 2, 6}
	px := []int{0, 5, 0, 2, 3}
	py := []int{0, 4, 0, 3, 3}
	rx := []int{0, 4, 1, 4, 0}
	ry := []int{1, 5, 2, 4, 3}
	for i := range x {
		c := core.Cell{X: x[i], Y: y[i]}
		p := core.Cell{X: px[i], Y: py[i]}
		r := core.Cell{X: rx[i], Y: ry[i]}
		c1 := c.Rotate(p, core.DirCW)
		c2 := c1.Rotate(p, core.DirCCW)
		if c1.X != r.X || c1.Y != r.Y {
			t.Fatalf("wrong CW rotation of %v around %v: expected %v, received %v",
				c, p, r, c1)
		}
		if c2.X != c.X || c2.Y != c.Y {
			t.Fatalf("wrong CCW rotation of %v around %v: expected %v, received %v",
				c1, p, c, c2)
		}
	}
}
