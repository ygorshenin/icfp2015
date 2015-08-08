package core

func PlayProblem(p Problem, phrases []string, outputCh chan []OutputEntry) {
	//if p.SourceLength != len(p.SourceSeeds) {
	//	panic("bad sourceLength")
	//}
	solCh := make(chan OutputEntry)
	for _, seed := range p.SourceSeeds {
		b := NewBoard(p.Height, p.Width, seed, p.Filled, p.Units, phrases)
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
	outputCh <- OutputEntry{
		ProblemId: -1,
		Seed:      b.rng.seed,
		Tag:       "",
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

func (b *Board) PlayGreedilyNoRotations() {
	was := b.BoolSlice()
	freezeDir := b.IntSlice()
	prev := b.CellPtrSlice()
	child := make([][][]*Cell, 4)
	for dir := 0; dir < 4; dir++ {
		child[dir] = b.CellPtrSlice()
	}

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
				break
			}
			if err != nil {
				panic(err)
			}
		}

		b.RemoveActiveUnit()

		topLeft := b.activeUnit.TopLeftCell()
		was[topLeft.X][topLeft.Y] = true
		var qx, qy []int
		qt := 0
		qx = append(qx, topLeft.X)
		qy = append(qy, topLeft.Y)
		for qt < len(qx) {
			x, y := qx[qt], qy[qt]
			b.activeUnit.Shift(x, y)
			qt++
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
			b.activeUnit.Shift(-x, -y)
		}

		bestX, bestY, bestScore := -1, -1, -1
		for x := 0; x < b.width; x++ {
			for y := 0; y < b.height; y++ {
				if freezeDir[x][y] < 0 {
					continue
				}
				b.activeUnit.Shift(x, y)
				b.AddActiveUnit()
				score := 0
				for _, c := range b.activeUnit.Cells {
					full := true
					for x := 0; x < b.width; x++ {
						if !b.occupied[x][c.Y] {
							full = false
							break
						}
					}
					if full {
						score++
					}
				}
				score += y // the lower the better
				b.RemoveActiveUnit()
				b.activeUnit.Shift(-x, -y)
				if bestScore < score {
					bestX, bestY, bestScore = x, y, score
				}
			}
		}

		if bestX < 0 {
			panic("greedy: cannot move")
		}

		b.AddActiveUnit()

		var pathX, pathY []int
		pathX = append(pathX, bestX)
		pathY = append(pathY, bestY)
		for x, y := bestX, bestY; ; {
			p := prev[x][y]
			if p == nil {
				break
			}
			pathX = append(pathX, p.X)
			pathY = append(pathY, p.Y)
			x, y = p.X, p.Y
		}

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
			cmd := Command{dir: Direction(dir), letter: directionLetters[dir][0]}
			b.MoveActiveUnit(cmd)
		}
	}
}
