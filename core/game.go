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
	b.PlayGreedily()

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

type State struct {
	x, y int
	rot  int
}

func (s *State) String() string {
	return fmt.Sprintf("x=%d y=%d rot=%d", s.x, s.y, s.rot)
}

func (b *Board) PlayGreedily() {
	unitsPlaced := 0
	for {
		initRowCC := calcRowsCC(b)
		// Total initial number of connected components.
		initNumCC := calcCC(b)
		was := make(map[State]struct{})
		freezeDir := make(map[State]int)
		prev := make(map[State]State)
		var child [6]map[State]State
		for i := 0; i < 6; i++ {
			child[i] = make(map[State]State)
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
		maxRotations := b.activeUnit.SubgroupOrder()

		if err := b.RemoveActiveUnit(); err != nil {
			panic("greedy: " + err.Error())
		}
		start := State{x: b.activeUnit.Pivot.X, y: b.activeUnit.Pivot.Y, rot: 0}
		was[start] = struct{}{}
		var q []State
		q = append(q, start)
		for qt := 0; qt < len(q); qt++ {
			st := q[qt]
			x, y, rot := st.x, st.y, st.rot
			b.activeUnit.Shift(start.x, start.y, x, y)
			b.activeUnit.RotateNTimes(+rot)
			for dir := 0; dir < 6; dir++ {
				var newActiveUnit *Unit
				if dir < 4 {
					newActiveUnit = b.activeUnit.Move(Direction(dir))
				} else {
					newActiveUnit = b.activeUnit.Rotate(Direction(dir))
				}
				if !b.CanPlace(newActiveUnit) {
					freezeDir[st] = dir
					continue
				}
				nx, ny, nrot := newActiveUnit.Pivot.X, newActiveUnit.Pivot.Y, rot
				if Direction(dir) == DirCW {
					nrot--
					if nrot < 0 {
						nrot += maxRotations
					}
				}
				if Direction(dir) == DirCCW {
					nrot++
					if nrot >= maxRotations {
						nrot -= maxRotations
					}
				}
				nst := State{nx, ny, nrot}
				if _, ok := was[nst]; ok {
					continue
				}
				was[nst] = struct{}{}
				q = append(q, nst)
				child[dir][st] = nst
				prev[nst] = st
			}
			b.activeUnit.RotateNTimes(-rot)
			b.activeUnit.Shift(x, y, start.x, start.y)

		}

		bestState, bestScore, anyMove := State{}, 0, false
		states := 0
		for _, st := range q {
			x, y, rot := st.x, st.y, st.rot
			if _, ok := freezeDir[st]; !ok {
				continue
			}
			states++
			b.activeUnit.Shift(start.x, start.y, x, y)
			b.activeUnit.RotateNTimes(+rot)
			if err := b.AddActiveUnit(); err != nil {
				panic(err)
			}

			score := calcScoreAfterUnitLock(b, initRowCC, initNumCC)
			if !anyMove || bestScore < score {
				bestState, bestScore, anyMove = st, score, true
			}

			if err := b.RemoveActiveUnit(); err != nil {
				panic(err)
			}
			b.activeUnit.RotateNTimes(-rot)
			b.activeUnit.Shift(x, y, start.x, start.y)
		}
		if !anyMove {
			panic("greedy: cannot move")
		}

		if err := b.AddActiveUnit(); err != nil {
			panic("greedy: " + err.Error())
		}

		var path []State
		for st := bestState; ; {
			path = append(path, st)
			p, ok := prev[st]
			if !ok {
				break
			}
			st = p
		}
		for i := len(path) - 1; i >= 0; i-- {
			st := path[i]
			var dir int
			if i == 0 {
				dir = freezeDir[st]
			} else {
				for dir = 0; dir < 6; dir++ {
					ch, ok := child[dir][st]
					if ok && ch == path[i-1] {
						break
					}
				}
			}
			if dir == 6 {
				panic("could not find an edge to the next state in path")
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
