#!/bin/ksh

#Compute the ice concentration autocorrelations for a month at a time
#through the year.

PSGDIR=../psg

for pole in north
do
  p=`echo $pole | cut -c1`
  for base in 9601 9602 9603 9604 9605 9606 9607 9608 9609 9610 9611 9612
  do
    for dd in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 \
              21 22 23 24 25 26 27 28 29 30 31
    do
      if [ ! -s $pole.$base$dd ] ; then
        wgrib $PSGDIR/${p}psg.$base | grep $base$dd | wgrib -i $PSGDIR/${p}psg.$base -nh -o ${pole}.$base$dd
      fi
    done
    rm $PSGDIR/${p}psg.$base
   
    ./acor${p} $base $p
    ./findacn $base 125.
  done
done 
