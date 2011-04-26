#!/bin/bash
# Get load time of a bunch of sites (sequentially)
for url in $*; do
    curl -s -o /dev/null -w "loadtime %{time_total}\n" "$url"
done
echo "."
