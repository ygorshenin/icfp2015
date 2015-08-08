package core

import "errors"

var GameOver = errors.New("Game over")
var PositionRepeated = errors.New("move leads into a previously seen state")

type BoardCache struct {
	seen map[uint64]struct{}
}

func (bc *BoardCache) ComputeHash(b *Board) uint64 {
	var h uint64
	for r := 0; r < b.height; r++ {
		for c := 0; c < b.width; c++ {
			h = h * 1000000007
			if b.occupied[r][c] {
				h++
			}
		}
	}
	return h
}

func (bc *BoardCache) Add(b *Board) {
	h := bc.ComputeHash(b)
	bc.seen[h] = struct{}{}
}

// Assuming no collisions.
func (bc *BoardCache) Seen(b *Board) bool {
	h := bc.ComputeHash(b)
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
