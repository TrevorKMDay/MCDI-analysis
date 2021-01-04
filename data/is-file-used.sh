#!/bin/bash

files=$(find . \
			-type  f \
			\(  -name "*.csv" -or \
			 	-name "*.RDS"  -or \
			 	-name "*.rds"  -or \
			 	-name "*.xlsx?" \) )

for f in ${files} ; do

	bn=$(basename ${f})
	if grep -q ${bn} ../code/*/*.R ; then
		echo "${bn} is used in at least one R script."
	else
		echo "No use of ${f} found."
	fi

done