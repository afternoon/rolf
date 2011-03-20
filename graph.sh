#!/bin/sh
rrdtool graph graph.png --start 1300650000 --step 10 \
        'DEF:loadtime=priv/data/frank@josie/loadtime.rrd:loadtime:AVERAGE' \
        'AREA:loadtime#ff0000:loadtime'
open graph.png
