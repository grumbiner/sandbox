#!/bin/bash
for dy in 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
do
  time ./qc.jcl 9712$dy 
done
for dy in 01 02 03 04 05 06 07 08 09 10 11 12
do
  time ./qc.jcl 9801$dy 
done
