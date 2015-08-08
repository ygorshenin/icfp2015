package core

func PlayProblem(p Problem, phrases []string, outputCh chan []OutputEntry) {
	//if p.SourceLength != len(p.SourceSeeds) {
	//	panic("bad sourceLength")
	//}
	solCh := make(chan OutputEntry)
	for _, seed := range p.SourceSeeds {
		b := NewBoard(p.Height, p.Width, seed, p.Filled, p.Units, phrases)
		go b.Play(solCh)
	}
	var output []OutputEntry
	for range p.SourceSeeds {
		o := <-solCh
		o.ProblemId = p.Id // board does not need to know it
		output = append(output, o)
	}
	outputCh <- output
}

func (b *Board) Play(outputCh chan OutputEntry) {
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
	outputCh <- OutputEntry{
		ProblemId: -1,
		Seed:      b.rng.seed,
		Tag:       "",
		Solution:  b.gameLog,
	}
}
