package core_test

import (
	"testing"

	"../core"
)

func TestRng(t *testing.T) {
	r := core.NewRng(17)
	expected := []int{0, 24107, 16552, 12125, 9427, 13152, 21440, 3383, 6873, 16117}
	received := make([]int, len(expected))
	for i := range expected {
		received[i] = r.NextRand()
	}
	for i := range expected {
		if expected[i] != received[i] {
			t.Fatalf("wrong rng:\nexpected %v,\nreceived %v", expected, received)
		}
	}
}
