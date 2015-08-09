#!/bin/bash

case $# in
     1) ;; 
     *) echo "Usage: $0 test-num" 2>&1
        exit -1
esac

readonly ROOT="$(dirname $0)"

readonly INFILE="$ROOT/problems/$1.json"
readonly OUTFILE="$ROOT/problems/$1.out.json"
readonly RENDERER="$ROOT/renderer/Main"
readonly PLAYER="$ROOT/player/main.go"

if [ ! -f "$INFILE" ]
then
    echo "Can't find $INFILE." 2>&1
    exit -1
fi

OUTPUT=$(< "$INFILE")

set -e

go run $PLAYER -f "$INFILE" -o "$OUTFILE" --scores --v "$RENDERER"