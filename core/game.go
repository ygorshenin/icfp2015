package core

import (
	"fmt"
	"time"
)

func PlayProblem(p Problem, phrases []string, outputCh chan []OutputEntry) {
	solCh := make(chan OutputEntry)
	for _, seed := range p.SourceSeeds {
		b := NewBoard(p.Height, p.Width, seed, p.Filled, p.Units, phrases, p.SourceLength)
		go b.PlayAndReport(solCh)
	}
	var output []OutputEntry
	for range p.SourceSeeds {
		o := <-solCh
		o.ProblemId = p.Id // board does not need to know it
		output = append(output, o)
	}
	outputCh <- output
}

func (b *Board) PlayAndReport(outputCh chan OutputEntry) {
	b.PlayGreedilyNoRotations()
	phrasesScore := PowerScores(b.gameLog, b.phrases)
	longTag := fmt.Sprintf("Total score: %d (%d moves + %d phrases).",
		b.movesScore+phrasesScore, b.movesScore, phrasesScore)
	tag := fmt.Sprint(time.Now().Format(time.Stamp))
	_ = longTag
	outputCh <- OutputEntry{
		ProblemId: -1,
		Seed:      b.rng.seed,
		Tag:       tag,
		Solution:  b.gameLog,
	}
}

func (b *Board) PlayRandomly() {
	rng := NewRng(0)
	for {
		dir := Direction(rng.NextRand() % 4) // no rotations
		letter := directionLetters[dir][0]
		cmd := Command{dir: dir, letter: letter}
		err := b.MoveActiveUnit(cmd)
		if err == GameOver {
			break
		}
	}
}

func calcScoreAfterUnitLock(b *Board) int {
	score := 0
	fullRows := 0
	occupiedTotal := 0
	nonEmptyRows := 0

	for y := 0; y < b.height; y++ {
		occupiedInRow := 0
		for x := 0; x < b.width; x++ {
			if b.occupied[x][y] {
				occupiedInRow++
				occupiedTotal++
			}
		}
		if occupiedInRow > 0 {
			nonEmptyRows++
		}
		if occupiedInRow == b.width {
			score += 1000
			fullRows++
		}
	}
	score += 5 * b.activeUnit.TopLeftCell().Y // the lower the better
	return score
}

func (b *Board) PlayGreedilyNoRotations() {
	was := b.BoolSlice()
	freezeDir := b.IntSlice()
	prev := b.CellPtrSlice()
	child := make([][][]*Cell, 4)
	for dir := 0; dir < 4; dir++ {
		child[dir] = b.CellPtrSlice()
	}

	unitsPlaced := 0
	for {
		for x := 0; x < b.width; x++ {
			for y := 0; y < b.height; y++ {
				was[x][y] = false
				freezeDir[x][y] = -1
				prev[x][y] = nil
				for dir := 0; dir < 4; dir++ {
					child[dir][x][y] = nil
				}
			}
		}
		if b.activeUnit == nil {
			err := b.Spawn()
			if err == GameOver {
				return
			}
			if err != nil {
				panic(err)
			}
		}

		if err := b.RemoveActiveUnit(); err != nil {
			panic("greedy: " + err.Error())
		}

		topLeft := b.activeUnit.TopLeftCell()
		sx, sy := topLeft.X, topLeft.Y
		was[sx][sy] = true
		var qx, qy []int
		qt := 0
		qx = append(qx, sx)
		qy = append(qy, sy)
		for qt < len(qx) {
			x, y := qx[qt], qy[qt]
			qt++
			b.activeUnit.Shift(sx, sy, x, y)
			for dir := 0; dir < 4; dir++ {
				newActiveUnit := b.activeUnit.Move(Direction(dir))
				if !b.CanPlace(newActiveUnit) {
					freezeDir[x][y] = dir
					continue
				}
				newTopLeft := newActiveUnit.TopLeftCell()
				nx, ny := newTopLeft.X, newTopLeft.Y
				if was[nx][ny] {
					continue
				}
				was[nx][ny] = true
				qx = append(qx, nx)
				qy = append(qy, ny)
				child[dir][x][y] = &Cell{X: nx, Y: ny}
				prev[nx][ny] = &Cell{X: x, Y: y}
			}
			b.activeUnit.Shift(x, y, sx, sy)

		}

		bestX, bestY, bestScore, anyMove := 0, 0, 0, false
		states := 0
		for qi := range qx {
			x, y := qx[qi], qy[qi]
			if freezeDir[x][y] < 0 {
				continue
			}
			states++
			b.activeUnit.Shift(sx, sy, x, y)
			if err := b.AddActiveUnit(); err != nil {
				panic(err)
			}

			score := calcScoreAfterUnitLock(b)
			if !anyMove || bestScore < score {
				bestX, bestY, bestScore, anyMove = x, y, score, true
			}

			if err := b.RemoveActiveUnit(); err != nil {
				panic(err)
			}
			b.activeUnit.Shift(x, y, sx, sy)
		}
		if !anyMove {
			panic("greedy: cannot move")
		}

		if err := b.AddActiveUnit(); err != nil {
			panic("greedy: " + err.Error())
		}

		var pathX, pathY []int
		for x, y := bestX, bestY; ; {
			pathX = append(pathX, x)
			pathY = append(pathY, y)
			p := prev[x][y]
			if p == nil {
				break
			}
			x, y = p.X, p.Y
		}
		// fmt.Println(pathX, pathY)
		for i := len(pathX) - 1; i >= 0; i-- {
			x, y := pathX[i], pathY[i]
			var dir int
			if i == 0 {
				dir = freezeDir[x][y]
			} else {
				for dir = 0; dir < 4; dir++ {
					ch := child[dir][x][y]
					if ch != nil && ch.X == pathX[i-1] && ch.Y == pathY[i-1] {
						break
					}
				}
			}
			if dir == 4 {
				panic("rotations are not allowed in this solution!")
			}
			cmd := Command{dir: Direction(dir), letter: directionLetters[dir][0]}
			err := b.MoveActiveUnit(cmd)
			if err == GameOver {
				unitsPlaced++
				return
			}
			if err != nil {
				panic(err)
				return
			}
		}
		unitsPlaced++
	}
	_ = unitsPlaced
}
