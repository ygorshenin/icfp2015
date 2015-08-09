#!/bin/bash

case $# in
     1) ;; 
     *) echo "Usage: $0 test-num" 2>&1
        exit -1
esac

readonly API_TOKEN="M727ZAGLQCZeGWSTOwJ/eAZNHdClyz70/foXubiieKc="
readonly TEAM_ID=292
readonly ROOT="$(dirname $0)"
readonly PROBLEMS="ROOT/problems"

readonly FILE="$ROOT/problems/$1.out.json"
if [ ! -f "$FILE" ]
then
    echo "Can't find $FILE." 2>&1
    exit -1
fi

OUTPUT=$(< "$FILE")

set -e
curl --user :$API_TOKEN -X POST -H "Content-Type: application/json" \
     -d "$OUTPUT" \
     https://davar.icfpcontest.org/teams/$TEAM_ID/solutions
