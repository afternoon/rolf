#!/bin/bash
now=`date +%s`
start=`expr $now - 3600`
rrdtool graph rolf_loadtime.png -s $start -S 10 -w 800 -h 600 -l 0 -u 1.5 \
        -c BACK#191919 -c CANVAS#191919 -c SHADEA#191919 -c SHADEB#191919 -c FONT#ffffff \
        'DEF:loadtime=priv/data/rolf@josie/loadtime.rrd:loadtime:AVERAGE' \
        'AREA:loadtime#0091ff:loadtime'
rrdtool graph rolf_freespace.png -s $start -S 300 -w 800 -h 600 -l 0 -u 100 \
        -c BACK#191919 -c CANVAS#191919 -c SHADEA#191919 -c SHADEB#191919 -c FONT#ffffff \
        'DEF:freespace=priv/data/rolf@josie/disk.rrd:freespace:AVERAGE' \
        'AREA:freespace#0091ff:freespace'
rrdtool graph rolf2_loadtime.png -s $start -S 10 -w 800 -h 600 -l 0 -u 1.5 \
        -c BACK#191919 -c CANVAS#191919 -c SHADEA#191919 -c SHADEB#191919 -c FONT#ffffff \
        'DEF:loadtime=priv/data/rolf2@josie/loadtime.rrd:loadtime:AVERAGE' \
        'AREA:loadtime#0091ff:loadtime'
open rolf_freespace.png rolf_loadtime.png rolf2_loadtime.png
