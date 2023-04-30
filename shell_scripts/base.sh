#!/bin/sh
#Carry out basic statistics for month:
#Robert Grumbine 1 December 2000

#echo $1 $2
set -x
pwd
tag=$1
cat httpd-log80.${tag}* | grep -v HEAD | grep -v '\] - \[' > $tag
getstats -M -d -w -m -hs -l $tag      > $tag.stat1      
getstats -M -ra -fa -dt -dl 3 -l $tag > $tag.stat2 
