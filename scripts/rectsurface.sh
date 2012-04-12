#!/bin/bash

minx=$1
maxx=$2
miny=$3
maxy=$4

countx=`expr $(($maxx - $minx + 1))`
county=`expr $(($maxy - $miny + 1))`

count=`expr $(($countx * $county))`

echo "2 RectGen"

echo $count

for x in `seq $minx $maxx`;
do
	for y in `seq $miny $maxy`;
	do
		echo $x $y;
	done
done
