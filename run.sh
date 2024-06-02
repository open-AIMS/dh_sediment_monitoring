#!/bin/sh

git clone https://github.com/open-AIMS/dh_sediment_monitoring.git ../project1/temp
\cp -fr ../project1/temp/R ../project/R
\cp -fr ../project1/temp/md ../project/md
\cp -fr ../project1/temp/parameters ../project/parameters
rm -R ../project1/temp
cd /home/project
cd R
Rscript run.R

