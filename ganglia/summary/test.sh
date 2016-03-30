#!/bin/bash

timestamps=$(ls /mnt/ganglia_backup | grep 06)
echo $timestamps


for D in $timestamps$timestamps
do
	echo $D
	hour=${D:0:2}
	min=${D:3:2}
	mon=${D:6:2}
	day=${D:9:2}
	year=${D:12:4}
	timestamp=$(date -d "$year-$mon-$day $hour:$min" +"%s")
	echo $timestamp
	nodes=$(ls /mnt/ganglia_backup/$D/Spark)
	for node in $nodes
	do
		echo $node
		rrdtool fetch /mnt/ganglia_backup/$D/Spark/$node/bytes_out.rrd AVERAGE -e $timestamp >> /mnt/ganglia_backup/summary/$node.csv
	done
	
done


