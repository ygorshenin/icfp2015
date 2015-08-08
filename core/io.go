package core

import (
	"encoding/json"
	"errors"
	"io"
	"io/ioutil"
	"os"
)

type Problem struct {
	Id           int
	Units        []Unit
	Width        int
	Height       int
	Filled       []Cell
	SourceLength int
	SourceSeeds  []uint64
}

type Cell struct {
	X, Y int
}

type Unit struct {
	Cells []Cell `json:"members"`
	Pivot Cell
}

type OutputEntry struct {
	ProblemId int    `json:"problemId"`
	Seed      uint64 `json:"seed"`
	Tag       string `json:"tag"`
	Solution  string `json:"solution"`
}
type Output []OutputEntry

func ReadProblem(filename string) (Problem, error) {
	var p Problem
	f, err := os.Open(filename)
	if err != nil {
		return p, errors.New("reading problem: " + err.Error())
	}
	buf, err := ioutil.ReadAll(f)
	if err != nil {
		return p, errors.New("reading problem: " + err.Error())
	}
	err = json.Unmarshal(buf, &p)
	if err != nil {
		return p, errors.New("reading problem: " + err.Error())
	}
	return p, nil
}

func WriteOutput(o Output, w io.Writer) error {
	b, err := json.Marshal(o)
	if err != nil {
		return err
	}
	_, err = w.Write(b)
	if err != nil {
		return errors.New("writing output: " + err.Error())
	}
	return nil
}

func WriteOutputToFile(o Output, filename string) {
	f, err := os.Create(filename)
	if err != nil {
		panic(err)
	}
	WriteOutput(o, f)
}
