#!/bin/sh
#Carry out basic statistics for month:
#Robert Grumbine 1 February 2001

tag=$1
getstats -M -d -w -m -hs -l $tag      > $tag.stat1      
getstats -M -ra -fa -dt -dl 3 -l $tag > $tag.stat2 
