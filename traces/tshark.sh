#!/bin/bash
for file in *; do

	echo $file
	tshark -r $file -T fields -e ip.src -e ip.dst -e frame.len -e frame.time_relative -E header=y -E separator=, > ../univ1_csv/$file.csv
done;
