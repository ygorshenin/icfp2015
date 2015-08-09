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

	tag := fmt.Sprint(time.Now().Format(time.Stamp))
	outputCh <- OutputEntry{
		ProblemId:    -1,
		Seed:         b.rng.seed,
		Tag:          tag,
		Solution:     b.gameLog,
		MovesScore:   b.movesScore,
		PhrasesScore: PowerScores(b.gameLog, b.phrases),
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

func calcCC(b *Board) int {
	was := b.BoolSlice()

	size := b.width * b.height
	qx, qy := make([]int, size), make([]int, size)

	numCC := 0
	for x := 0; x < b.width; x++ {
		for y := 0; y < b.height; y++ {
			if !b.occupied[x][y] && !was[x][y] {
				numCC++
				qh, qt := 0, 0
				qx[qt], qy[qt] = x, y
				qt++
				was[x][y] = true
				for qh != qt {
					pivot := Cell{X: qx[qh], Y: qy[qh]}
					qh++
					next := Cell{X: pivot.X + 1, Y: pivot.Y}
					for i := 0; i < 6; i++ {
						cur := next.Rotate(pivot, DirCW)
						cx, cy := cur.X, cur.Y
						if cx >= 0 && cx < b.width && cy >= 0 && cy < b.height && !was[cx][cy] {
							was[cx][cy] = true
							qx[qt], qy[qt] = cx, cy
							qt++
						}
					}
				}
			}
		}
	}

	return numCC
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
func calcScoreAfterUnitLock(b *Board, initRowCC []int, initNumCC int) int {
	score := 0
	currRowCC := calcRowsCC(b)

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

		// Penalty for new connected components in rows.
		newCC := currRowCC[y] - initRowCC[y]
		if newCC > 0 {
			score -= 10 * newCC
		}
	}

	y := b.activeUnit.TopLeftCell().Y
	score += 100 * y

	// +- 20 for each connected component
	diffCC := initNumCC - calcCC(b)
	score += 20 * diffCC

	// Probably not good optimization since we alredy computing SCCs.
	// score -= numEmptyCellsBelowActiveUnit(b)

	// When number of free rows is low enough,
	// place figures close to bounds.
	if y < 5 {
		minX, maxX := getUnitBoundsX(b.activeUnit)
		toLeft, toRight := minX, b.width-maxX-1
		if toLeft > toRight {
			score -= 10 * toRight
		} else {
			score -= 10 * toLeft
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

	unitsPlaced := 0
	for {
		// Initial number of connected components per each row.
		initRowCC := calcRowsCC(b)
		// Total initial number of connected components.
		initNumCC := calcCC(b)

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

			score := calcScoreAfterUnitLock(b, initRowCC, initNumCC)
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
