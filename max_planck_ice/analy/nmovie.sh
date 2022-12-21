#!/bin/ksh

for base in 9810 9811 9812 9901
do
  wgrib north.$base.grb | grep ICETK > nindex

  for dy in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 \
            21 22 23 24 25 26 27 28 29 30 31 
  do
    for hh in 00 06 12 18
    do
      if [ -f nout ] ; then
        rm nout
      fi
      grep ${base}$dy$hh nindex | wgrib -i north.$base.grb -nh -o nout
      if [ -s nout ] ; then
        ./nthickxpm nout nout.land nt${base}${dy}${hh}.xpm
        xpmtogif nt${base}${dy}${hh}.xpm
        rm nt${base}${dy}${hh}.xpm
      fi
    done
  done

  ls -1 nt${base}*.gif > beta
  whirlgif -o nthick.$base.gif -i beta

done
