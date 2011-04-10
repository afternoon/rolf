#!/bin/bash
# Get load time of http://aftnn.org/
curl -s -o /dev/null -w "loadtime %{time_total}\n" "http://aftnn.org"
echo "."
