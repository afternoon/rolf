#!/bin/bash
now=`date +%s`
start=`expr $now - 3600`
rrdtool graph /var/www/aftnn/stuff/rolf_loadtime.png -s $start -S 10 \
        -w 1359 -h 851 -l 0 -u 1000 -r \
        -c BACK#191919 -c CANVAS#191919 -c SHADEA#191919 -c SHADEB#191919 -c FONT#ffffff \
        'DEF:bbc=data/rolf@127.0.0.1/loadtime.rrd:bbc:AVERAGE' \
        'DEF:guardian=data/rolf@127.0.0.1/loadtime.rrd:guardian:AVERAGE' \
        'DEF:lastminute=data/rolf@127.0.0.1/loadtime.rrd:lastminute:AVERAGE' \
        'DEF:aws=data/rolf@127.0.0.1/loadtime.rrd:aws:AVERAGE' \
        'DEF:appengine=data/rolf@127.0.0.1/loadtime.rrd:appengine:AVERAGE' \
        'DEF:twitter=data/rolf@127.0.0.1/loadtime.rrd:twitter:AVERAGE' \
        'DEF:argos=data/rolf@127.0.0.1/loadtime.rrd:argos:AVERAGE' \
        'DEF:aarouteplanner=data/rolf@127.0.0.1/loadtime.rrd:aarouteplanner:AVERAGE' \
        'DEF:ocado=data/rolf@127.0.0.1/loadtime.rrd:ocado:AVERAGE' \
        'DEF:dailymail=data/rolf@127.0.0.1/loadtime.rrd:dailymail:AVERAGE' \
        'LINE:bbc#0091ff' \
        'LINE:guardian#91ff00' \
        'LINE:lastminute#9100ff' \
        'LINE:aws#91ff00' \
        'LINE:appengine#ff0091' \
        'LINE:twitter#ff9100' \
        'LINE:argos#00ff91' \
        'LINE:aarouteplanner#cc0066' \
        'LINE:ocado#0066cc' \
        'LINE:dailymail#66cc00'
