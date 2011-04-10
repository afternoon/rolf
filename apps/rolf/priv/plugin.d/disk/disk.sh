#!/bin/bash
# Get free space on root filesystem
echo -n "freespace "; df -h / | tail -n 1 | awk '{ print $5 }' | cut -d % -f 1
echo "."
