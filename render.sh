#!/bin/bash

case $# in
     1) ;; 
     *) echo "Usage: $0 test-num" 2>&1
        exit -1
esac

readonly ROOT="$(dirname $0)"

if ! (cd "$ROOT/renderer" ; ghc Main.hs)
then
    echo "Can't update renderer." 2>&1
    exit -1
fi

readonly IN="$ROOT/problems/$1.json"
readonly OUT="$ROOT/problems/$1.out.json"

if [ ! -f "$IN" ]
then
    echo "Can't find input file: $IN" 2>&1
    exit -1
fi

if [ ! -f "$OUT" ]
then
    echo "Can't find output file: $OUT" 2>&1
    exit -1
fi

"$ROOT/renderer/Main" -i "$IN" -o "$OUT" -w 1600 -h 1200
