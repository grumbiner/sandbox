#!/bin/sh
#Compute summary statistics for critical concentration level and lead for 
#  contingency table stats
#Robert Grumbine 17 May 2018

make summary1

for level in 0.15 0.75 0.95
do
  for fh in 000 024 048 072 096 120 144 168 192
  do
    grep "nhlevel $level " scoreout.*.$fh > nh$fh.$level
    ./summary1 nh$fh.$level > nhsummary.$fh.$level
  done
  grep ' 10 ' nhsummary.*.$level > bias.$level
  grep ' 9 ' nhsummary.*.$level > csi.$level
  grep ' 8 ' nhsummary.*.$level > pct.$level
  grep ' 7 ' nhsummary.*.$level > fcr.$level
  grep ' 6 ' nhsummary.*.$level > far.$level
  grep ' 5 ' nhsummary.*.$level > pod.$level

done
