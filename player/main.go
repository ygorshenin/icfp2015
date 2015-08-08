package main

import (
	"flag"
	"fmt"
	"os"
	"runtime"

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
	processorCores = flag.Int("c", runtime.NumCPU(), "Number of processor cores available")
	phrases        strings
	outfile        = flag.String("o", "", "Name of the file where output should be stored.")
)

func init() {
	flag.Var(&filenames, "f", "Files containing JSON encoded input")
	flag.Var(&phrases, "p", "Phrases of power")
}

func main() {
	flag.Parse()
	runtime.GOMAXPROCS(*processorCores)
	outputCh := make(chan []core.OutputEntry)
	for _, filename := range filenames {
		p, err := core.ReadProblem(filename)
		if err != nil {
			panic(err)
		}
		go core.PlayProblem(p, phrases, outputCh)
	}
	var output []core.OutputEntry
	for range filenames {
		o := <-outputCh
		output = append(output, o...)
	}

	if *outfile != "" {
		f, err := os.Create(*outfile)
		if err != nil {
			panic(err)
		}
		core.WriteOutput(output, f)
	} else {
		core.WriteOutput(output, os.Stdout)
	}
}
