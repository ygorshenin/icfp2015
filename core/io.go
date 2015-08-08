package core

import (
	"encoding/json"
	"errors"
	"io"
	"io/ioutil"
	"os"
)

type Problem struct {
	id           int
	units        []Unit
	width        int
	height       int
	filled       []Cell
	sourceLength int
	sourceSeeds  []int
}

type Cell struct {
	x, y int
}

type Unit struct {
	cells []Cell `json:"members"`
	pivot Cell
}

type OutputEntry struct {
	ProblemId int    
	Seed      uint64    
	Tag       string 
	Solution  string 
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
