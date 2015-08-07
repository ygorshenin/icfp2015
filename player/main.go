package main

import (
	"flag"
	"fmt"
	"os"

	"../core"
)

type strings []string

func (s *strings) String() string {
	return fmt.Sprint(*s)
}

func (s *strings) Set(value string) error {
	*s = append(*s, value)
	return nil
}

var (
	filenames      strings
	timeLimit      = flag.Int("t", 0, "Time limit, in seconds, to produce output")
	memoryLimit    = flag.Int("m", 0, "Memory limit, in megabytes, to produce output")
	processorCores = flag.Int("c", 0, "Number of processor cores available")
	phrases        strings
)

func init() {
	flag.Var(&filenames, "f", "Files containing JSON encoded input")
	flag.Var(&phrases, "p", "Phrases of power")
}

func main() {
	flag.Parse()
	for _, filename := range filenames {
		p, err := core.ReadProblem(filename)
		if err != nil {
			panic(err)
		}
		fmt.Println(p.Id)
	}

	var o []core.OutputEntry
	o = append(o, core.OutputEntry{Tag: "test", Solution: "aaaaa"})
	core.WriteOutput(o, os.Stdout)
}
