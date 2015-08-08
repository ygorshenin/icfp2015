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
}

type Command struct {
	dir    Direction
	letter byte
}

func NewBoard(height, width int, seed uint64, initialOccupied []Cell, availableUnits []Unit, phrases []string) *Board {
	occupied := make([][]bool, height)
	for r := range occupied {
		occupied[r] = make([]bool, width)
	}
	for _, c := range initialOccupied {
		occupied[c.x][c.y] = true
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
	return c.x >= 0 && c.x < b.width && c.y >= 0 && c.y < b.height
}

func (b *Board) HasUnit(u *Unit) bool {
	for _, c := range u.cells {
		if !b.HasCell(c) {
			return false
		}
	}
	return true
}

func (c *Cell) Rotate(pivot Cell, dir Direction) Cell {
	r := Cell{x: c.x - pivot.x, y: c.y - pivot.y}
	// TODO
	r.x += pivot.x
	r.y += pivot.y
	return r
}

func (c *Cell) Move(dir Direction) Cell {
	switch dir {
	case DirE:
		return Cell{x: c.x + 1, y: c.y}
	case DirW:
		return Cell{x: c.x - 1, y: c.y}
	case DirSE:
		return Cell{x: c.x + 1, y: c.y + 1}
	case DirSW:
		return Cell{x: c.x - 1, y: c.y + 1}
	}
	panic("move cell: bad direction")
}

func (u *Unit) Rotate(dir Direction) *Unit {
	cells := make([]Cell, len(u.cells))
	for i, c := range u.cells {
		cells[i] = c.Rotate(u.pivot, dir)
	}
	return &Unit{
		cells: cells,
		pivot: u.pivot,
	}
}

func (u *Unit) Move(dir Direction) *Unit {
	switch dir {
	case DirCW:
		fallthrough
	case DirCCW:
		return u.Rotate(dir)
	}
	cells := make([]Cell, len(u.cells))
	for i, c := range u.cells {
		cells[i] = c.Move(dir)
	}
	return &Unit{
		cells: cells,
		pivot: u.pivot.Move(dir),
	}
}

func (c *Cell) String() string {
	return "(" + strconv.Itoa(c.x) + " " + strconv.Itoa(c.y) + ")"
}

func (u *Unit) String() string {
	s := "["
	for i, c := range u.cells {
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
func Spawn(b *Board) error {
	if b.activeUnit != nil {
		return errors.New("tried to generate a unit before freezing the previous one")
	}
	u := &b.availableUnits[b.rng.NextRand()%len(b.availableUnits)]
	cells := make([]Cell, len(u.cells))
	for i := range cells {
		cells[i] = u.cells[i]
	}
	pivot := u.pivot

	// TODO shift it

	// TODO GameOver

	b.activeUnit = &Unit{
		cells,
		pivot,
	}
	return nil
}

func (b *Board) RemoveActiveUnit() error {
	if b.activeUnit == nil {
		return errors.New("remove active: unit is nil")
	}
	for _, c := range b.activeUnit.cells {
		if !b.occupied[c.x][c.y] {
			return errors.New("tried to remove from an unoccupied cell " + c.String())
		}
		b.occupied[c.x][c.y] = false
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
	for _, c := range b.activeUnit.cells {
		if b.occupied[c.x][c.y] {
			return errors.New("tried to add to an occupied cell " + c.String())
		}
		b.occupied[c.x][c.y] = true
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
	for _, c := range u.cells {
		if b.occupied[c.x][c.y] {
			return false
		}
	}
	return true
}

func (b *Board) MoveActiveUnit(c Command) error {
	if b.activeUnit == nil {
		return errors.New("move: active unit not set")
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
