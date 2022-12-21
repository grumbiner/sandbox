#!/bin/sh

#Find regression parameters through time by month, per region
for mo in 01 02 03 04 05 06 07 08 09 10 11 12
do
  grep ' $mo ' spliced.out > $mo
  ./fit $mo 
done
