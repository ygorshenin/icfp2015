package main

import (
	"flag"
	"fmt"
	"os"
	"os/exec"
	"runtime"
	"time"

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
	timeLimit      = flag.Float64("t", 60, "Time limit, in seconds, to produce output.")
	memoryLimit    = flag.Int("m", 0, "Memory limit, in megabytes, to produce output.")
	processorCores = flag.Int("c", runtime.NumCPU(), "Number of processor cores available.")
	phrases        strings
	outfile        = flag.String("o", "", "Name of the file where output should be stored.")
	showScores     = flag.Bool("scores", false, "Scores are printed to stdout if set.")
	visualizerPath = flag.String("v", "", "Path to the visualizer.")
)

func init() {
	flag.Var(&filenames, "f", "Files containing JSON encoded input.")
	flag.Var(&phrases, "p", "Phrases of power.")
}

func main() {
	flag.Parse()
	if len(phrases) == 0 {
		phrases = strings(core.KnownPhrases)
	}
	core.StartTime = time.Now()
	core.TimeLimit = time.Duration(time.Second) * time.Duration(*timeLimit)
	runtime.GOMAXPROCS(*processorCores)
	outputCh := make(chan []core.OutputEntry)
	for _, filename := range filenames {
		p, err := core.ReadProblem(filename)
		if err != nil {
			panic(err)
		}
		go core.PlayProblem(p, phrases, outputCh)
		core.TimeSet <- true
	}

	var output []core.OutputEntry
	for range filenames {
		o := <-outputCh
		output = append(output, o...)
	}

	if *outfile != "" {
		core.WriteOutputToFile(output, *outfile)
	} else {
		core.WriteOutput(output, os.Stdout)
	}

	if *showScores {
		for _, o := range output {
			fmt.Printf("Total score: %d (%d moves + %d phrases).\n",
				o.MovesScore+o.PhrasesScore, o.MovesScore, o.PhrasesScore)
		}
	}

	if *visualizerPath != "" {
		if len(filenames) != 1 || *outfile == "" {
			fmt.Println("Provide exactly one file with -f and provide output file with -o in order to use visualizer.")
			return
		}
		cmd := exec.Command(*visualizerPath, "-w", "500", "-h", "500", "-i", filenames[0], "-o", *outfile)
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			panic("running visualizer: " + err.Error())
		}
	}
}
