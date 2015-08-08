package core

import (
	"errors"
	"strconv"
)

type Direction int

const (
	DirE Direction = iota
	DirW
	DirSE
	DirSW
	DirCW
	DirCCW
)

var directionLetters = []string{
	"bcefy2", // E
	"p'!.03", // W
	"lmno 5", // SE
	"aghij4", // SW
	"dqrvz1", // CW
	"kstuwx", // CCW
}

type Board struct {
	height, width  int
	occupied       [][]bool
	rng            *Rng
	activeUnit     *Unit
	gameLog        string
	availableUnits []Unit
	phrases        []string
	movesScore     int
	cache          *BoardCache
	problemId      int
}

type Command struct {
	dir    Direction
	letter byte
}

func NewBoard(height, width int, seed uint64, initialOccupied []Cell, availableUnits []Unit, phrases []string) *Board {
	occupied := make([][]bool, width)
	for r := range occupied {
		occupied[r] = make([]bool, height)
	}
	for _, c := range initialOccupied {
		occupied[c.X][c.Y] = true
	}
	return &Board{
		height:         height,
		width:          width,
		occupied:       occupied,
		rng:            NewRng(seed),
		activeUnit:     nil,
		gameLog:        "",
		availableUnits: availableUnits,
		phrases:        phrases,
		movesScore:     0,
		cache:          NewBoardCache(),
	}
}

func (b *Board) HasCell(c Cell) bool {
	return c.X >= 0 && c.X < b.width && c.Y >= 0 && c.Y < b.height
}

func (b *Board) HasUnit(u *Unit) bool {
	for _, c := range u.Cells {
		if !b.HasCell(c) {
			return false
		}
	}
	return true
}

func (c *Cell) Rotate(pivot Cell, dir Direction) Cell {
	r := Cell{X: c.X - pivot.X, Y: c.Y - pivot.Y}
	// TODO
	r.X += pivot.X
	r.Y += pivot.Y
	return r
}

func (c *Cell) Move(dir Direction) Cell {
	switch dir {
	case DirE:
		return Cell{X: c.X + 1, Y: c.Y}
	case DirW:
		return Cell{X: c.X - 1, Y: c.Y}
	case DirSE:
		return Cell{X: c.X + 1, Y: c.Y + 1}
	case DirSW:
		return Cell{X: c.X - 1, Y: c.Y + 1}
	}
	panic("move cell: bad direction")
}

func (u *Unit) Rotate(dir Direction) *Unit {
	cells := make([]Cell, len(u.Cells))
	for i, c := range u.Cells {
		cells[i] = c.Rotate(u.Pivot, dir)
	}
	return &Unit{
		Cells: cells,
		Pivot: u.Pivot,
	}
}

func (u *Unit) Move(dir Direction) *Unit {
	switch dir {
	case DirCW:
		fallthrough
	case DirCCW:
		return u.Rotate(dir)
	}
	cells := make([]Cell, len(u.Cells))
	for i, c := range u.Cells {
		cells[i] = c.Move(dir)
	}
	return &Unit{
		Cells: cells,
		Pivot: u.Pivot.Move(dir),
	}
}

func (c *Cell) String() string {
	return "(" + strconv.Itoa(c.X) + " " + strconv.Itoa(c.Y) + ")"
}

func (u *Unit) String() string {
	s := "["
	for i, c := range u.Cells {
		if i > 0 {
			s += " "
		}
		s += c.String()
	}
	s += "]"
	return s
}

// Spawn generates the next cell and places it to the board.
// b's availableUnits must stay unchanged.
func (b *Board) Spawn() error {
	if b.activeUnit != nil {
		return errors.New("tried to generate a unit before freezing the previous one")
	}
	u := &b.availableUnits[b.rng.NextRand()%len(b.availableUnits)]
	if len(u.Cells) == 0 {
		panic("empty unit in input")
	}

	minX := u.Cells[0].X
	maxX := u.Cells[0].X
	minY := u.Cells[0].Y
	for _, c := range u.Cells {
		x, y := c.X, c.Y
		if minX > x {
			minX = x
		}
		if maxX < x {
			maxX = x
		}
		if minY > y {
			minY = y
		}
	}
	var shiftX, shiftY int
	x := b.width - minX - maxX - 1
	if x < 0 {
		shiftX = (x + 1) / 2
	} else {
		shiftX = x / 2
	}
	shiftY = -minY

	cells := make([]Cell, len(u.Cells))
	for i := range cells {
		cells[i] = u.Cells[i]
		cells[i].X += shiftX
		cells[i].Y += shiftY
	}
	pivot := u.Pivot
	pivot.X += shiftX
	pivot.Y += shiftY
	b.activeUnit = &Unit{
		cells,
		pivot,
	}
	if !b.CanPlace(b.activeUnit) {
		return GameOver
	}
	b.AddActiveUnit()
	return nil
}

func (b *Board) RemoveActiveUnit() error {
	if b.activeUnit == nil {
		return errors.New("remove active: unit is nil")
	}
	for _, c := range b.activeUnit.Cells {
		if !b.occupied[c.X][c.Y] {
			return errors.New("tried to remove from an unoccupied cell " + c.String())
		}
		b.occupied[c.X][c.Y] = false
	}
	return nil
}

func (b *Board) AddActiveUnit() error {
	if b.activeUnit == nil {
		return errors.New("add active: unit is nil")
	}
	if !b.HasUnit(b.activeUnit) {
		return errors.New("tried to add an out-of-bounds unit")
	}
	for _, c := range b.activeUnit.Cells {
		if b.occupied[c.X][c.Y] {
			return errors.New("tried to add to an occupied cell " + c.String())
		}
		b.occupied[c.X][c.Y] = true
	}
	return nil
}

func (b *Board) CanPlace(u *Unit) bool {
	if u == nil {
		panic("tried to find a place for a nil unit")
	}
	if !b.HasUnit(u) {
		return false
	}
	for _, c := range u.Cells {
		if b.occupied[c.X][c.Y] {
			return false
		}
	}
	return true
}

func (b *Board) MoveActiveUnit(c Command) error {
	if b.activeUnit == nil {
		if err := b.Spawn(); err != nil {
			return err
		}
	}

	if err := b.RemoveActiveUnit(); err != nil {
		return err
	}
	oldActiveUnit := b.activeUnit
	newActiveUnit := b.activeUnit.Move(c.dir)
	if b.CanPlace(newActiveUnit) {
		b.activeUnit = newActiveUnit
		b.AddActiveUnit()
		if b.cache.Seen(b) {
			b.RemoveActiveUnit()
			b.activeUnit = oldActiveUnit
			b.AddActiveUnit()
			return PositionRepeated
		}
		b.cache.Add(b)
	} else {
		// frozen
		b.AddActiveUnit()
		b.activeUnit = nil
	}

	b.gameLog += string(c.letter)
	b.RemoveFullLines()
	return nil
}

// TODO do not forget to update cache and movesScore here
func (b *Board) RemoveFullLines() {
}
