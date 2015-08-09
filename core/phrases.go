package core

import (
	"time"
	"unicode"
)

var canonicalDirectionLetter [256]byte
var letterToDirectionMapping [256]Direction

func init() {
	for i := range letterToDirectionMapping {
		letterToDirectionMapping[i] = Direction(-1)
	}
	for i, s := range directionLetters {
		for j := range s {
			a := int(unicode.ToLower(rune(s[j])))
			b := int(unicode.ToUpper(rune(s[j])))
			canonicalDirectionLetter[a] = s[0]
			canonicalDirectionLetter[b] = s[0]
			letterToDirectionMapping[a] = Direction(i)
			letterToDirectionMapping[b] = Direction(i)
		}
	}
}

var KnownPhrases = []string{
	"ei!",
	//"ia!", // Looks like "ia!" and "ia! ia!" are not phrases after all.
	"r'lyeh",
	"yuggoth",
}

func OptimizeSolution(sol string, phrases []string) string {
	buf := []byte(sol)
	score := PowerScores(sol, phrases)
	optimizationStartTime := time.Now()
loop:
	for {
		if Timeout(optimizationStartTime, 500*time.Millisecond) {
			break
		}
		for _, phrase := range phrases {
			for i := 0; i+len(phrase) <= len(buf); i++ {
				if string(buf[i:i+len(phrase)]) == phrase {
					continue
				}
				if !PhraseMatches(buf[i:i+len(phrase)], phrase) {
					continue
				}
				nbuf := make([]byte, len(buf))
				copy(nbuf, buf)
				for j := 0; j < len(phrase); j++ {
					nbuf[i+j] = phrase[j]
				}
				nscore := PowerScores(string(nbuf), phrases)
				if score < nscore {
					score, buf = nscore, nbuf
					continue loop
				}

			}
		}
		break
	}
	return string(buf)
}

func PhraseMatches(s []byte, phrase string) bool {
	for i := 0; i < len(s); i++ {
		if canonicalDirectionLetter[int(s[i])] != canonicalDirectionLetter[int(phrase[i])] {
			return false
		}
	}
	return true
}

func PhraseToDirections(phrase string) []Direction {
	dirs := make([]Direction, len(phrase))
	for i, c := range phrase {
		dirs[i] = letterToDirectionMapping[int(c)]
		if dirs[i] < 0 {
			panic("bad letter")
		}
	}
	return dirs
}

func (b *Board) CanPronounceAtOnce(phrase string) bool {
	if b.activeUnit == nil {
		return false
	}
	u := b.activeUnit.Clone()
	b.RemoveActiveUnit()
	seen := make(map[State]struct{})
	seen[RelativePosition(b.activeUnit, u)] = struct{}{}
	for _, dir := range PhraseToDirections(phrase) {
		if dir < 4 {
			u = u.Move(Direction(dir))
		} else {
			u = u.Rotate(Direction(dir))
		}

		if !b.CanPlace(u) {
			b.AddActiveUnit()
			return false
		}
		st := RelativePosition(b.activeUnit, u)
		if _, ok := seen[st]; ok {
			b.AddActiveUnit()
			return false
		}
		seen[st] = struct{}{}
	}
	b.AddActiveUnit()
	return true
}
