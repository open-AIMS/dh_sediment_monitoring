#!/bin/sh

git clone https://github.com/open-AIMS/dh_sediment_monitoring.git ../project1/temp
cp ../project1/temp/R ../project/R
cp ../project1/temp/md ../project/md
rm -R ../project1/temp
cd R
Rscript run.R 
