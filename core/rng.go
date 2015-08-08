package core

type Rng struct {
	modulus    uint64
	multiplier uint64
	increment  uint64
	seed       uint64
	x          uint64
}

func NewRng(seed uint64) *Rng {
	return &Rng{
		modulus:    1 << 32,
		multiplier: 1103515245,
		increment:  12345,
		seed:       uint64(seed),
		x:          uint64(seed),
	}
}

func (r *Rng) NextRand() int {
	nx := (r.x*r.multiplier + r.increment) % r.modulus
	ret := r.x >> 16
	ret = ret & ((1 << 15) - 1)
	r.x = nx
	return int(ret)
}
