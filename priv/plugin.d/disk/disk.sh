#!/bin/bash
# Example rolf plugin
while read line; do
    echo -n "freespace "; df -h / | tail -n 1 | awk '{ print $5 }' | cut -d % -f 1
done
