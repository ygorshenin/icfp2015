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

func getUnitBoundsX(unit *Unit) (int, int) {
	minX, maxX := 999, 0
	for _, cell := range unit.Cells {
		x := cell.X
		if x < minX {
			minX = x
		}
		if x > maxX {
			maxX = x
		}
	}
	return minX, maxX
}

// Computes number of connected components in each row.
func calcRowsCC(b *Board) []int {
	rowsCC := make([]int, b.height)
	for y := 0; y < b.height; y++ {
		for x := 0; x < b.width; x++ {
			if b.occupied[x][y] && (x == 0 || !b.occupied[x-1][y]) {
				rowsCC[y]++
			}
		}
	}
	return rowsCC
}

// Calculates number of empty cells below all unit's cells.
func numEmptyCellsBelowActiveUnit(b *Board) int {
	unit := b.activeUnit
	emptyCells := 0
	for _, cell := range unit.Cells {
		x, y := cell.X, cell.Y
		if y+1 < b.height && !b.occupied[x][y+1] {
			emptyCells++
		}
	}
	return emptyCells
}

// Calculates score after unit placement.
func calcScoreAfterUnitLock(b *Board, initCC []int) int {
	score := 0
	currCC := calcRowsCC(b)

	for y := 0; y < b.height; y++ {
		occupiedInRow := 0
		for x := 0; x < b.width; x++ {
			if b.occupied[x][y] {
				occupiedInRow++
			}
		}
		if occupiedInRow == b.width {
			score += 1000
		}

		// Penalty for new connected components.
		newCC := currCC[y] - initCC[y]
		if newCC > 0 {
			score -= 50 * newCC
		}
	}

	y := b.activeUnit.TopLeftCell().Y
	score += 20 * y
	score -= 10 * numEmptyCellsBelowActiveUnit(b)

	// When number of free rows is low enough,
	// place figures close to bounds.
	if y < 5 {
		minX, maxX := getUnitBoundsX(b.activeUnit)
		toLeft, toRight := minX, b.width-maxX-1
		if toLeft > toRight {
			score -= 5 * toRight
		} else {
			score -= 5 * toLeft
		}
	}
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
	initCC := calcRowsCC(b)

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

			score := calcScoreAfterUnitLock(b, initCC)
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
