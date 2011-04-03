#!/bin/bash
# vim: ft=sh

BIN_DIR=`dirname $0`
BASE_DIR=`dirname $BIN_DIR`

cd $BASE_DIR

exec erl \
    -pa "${BASE_DIR}/ebin" \
    -noinput \
    -hidden \
    -sname rolf$$ \
    -s rolf_control \
    -extra "$@"
