#!/bin/bash
while read line; do
    curl -s -o /dev/null -w "loadtime %{time_total}\n" "http://aftnn.org"
    echo "."
done
