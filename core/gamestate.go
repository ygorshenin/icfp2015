package core

import (
	"errors"
	"sort"
)

var GameOver = errors.New("Game over")
var PositionRepeated = errors.New("move leads into a previously seen state")

type BoardCache struct {
	seen map[uint64]struct{}
}

func (bc *BoardCache) CellHash(c Cell) uint64 {
	return 5501*uint64(c.X) + uint64(c.Y)
}

func (bc *BoardCache) BoardHash(b *Board) uint64 {
	u := b.activeUnit
	if u == nil {
		return 0
	}
	cells := make([]Cell, len(u.Cells))
	for i := range u.Cells {
		cells[i] = Cell{X: u.Cells[i].X, Y: u.Cells[i].Y}
	}
	sort.Sort(Cells(cells))
	h := bc.CellHash(u.Pivot)
	for _, c := range cells {
		h = 1000000007*h + bc.CellHash(c)
	}
	return h
}

func (bc *BoardCache) Add(b *Board) {
	h := bc.BoardHash(b)
	bc.seen[h] = struct{}{}
}

// Assuming no collisions.
func (bc *BoardCache) Seen(b *Board) bool {
	h := bc.BoardHash(b)
	_, ok := bc.seen[h]
	return ok
}

func NewBoardCache() *BoardCache {
	return &BoardCache{
		seen: make(map[uint64]struct{}),
	}
}

func PowerScores(solution string, phrases []string) int {
	res := 0
	for _, p := range phrases {
		lenp := len(p)
		reps := 0
		for i := 0; i+len(p) <= len(solution); i++ {
			if solution[i:i+len(p)] == p {
				reps++
			}
		}
		bonus := 0
		if reps > 0 {
			bonus = 300
		}
		res += 2*lenp*reps + bonus
	}
	return res
}
