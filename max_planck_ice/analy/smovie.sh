#!/bin/ksh

base=9901
wgrib south.$base.grb | grep ICEC > sindex

for dy in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 \
          21 22 23 24 25 26 27 28 29 30 31
do
  for hh in 00 06 12 18
  do
    grep ${base}$dy$hh sindex | wgrib -i south.$base.grb -nh -o sout
    ./southmodxpm sout sout.land s${base}${dy}${hh}.xpm
    xpmtogif s${base}${dy}${hh}.xpm
    rm s${base}${dy}${hh}.xpm
  done
done

ls -1 s${base}*.gif > alpha
whirlgif -o south.jan99.gif -i alpha
