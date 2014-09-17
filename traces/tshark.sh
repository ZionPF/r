#!/bin/bash
for file in ./univ1_trace/*; do

	echo $file
	echo $file.csv
	#tshark -r $file -T fields -e ip.src -e ip.dst -e frame.len -e frame.time_relative -E header=y -E separator=, > ../$file.csv
done;
