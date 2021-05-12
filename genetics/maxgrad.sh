#!/bin/bash

for p in 125 250 500 1000
do
  g++ -O2 maxgrad.C -DPOPULATION=$p
  i=1
  while [ $i -le 10 ]
  do
    ./a.out $i > popgrad${p}.$i
    i=`expr $i + 1`
  done
  grep final popgrad${p}* > popgrad${p}
done
exit
g++ random.C -O2 -o random
i=1
while [ $i -le 10 ]
do
  ./random $i > random.$i
  i=`expr $i + 1`
done
grep final random.* > ranout
