#!/bin/ksh

for pole in north south
do
  p=`echo $pole | cut -c1`

  for base in 9902 9903 9904 9905 9906 9907 9908 9909
  do
    wgrib ${pole}.$base.grb | grep ICETK > ${p}index
  
    for dy in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 \
              21 22 23 24 25 26 27 28 29 30 31 
    do
      for hh in 00 06 12 18
      do
        if [ -f ${p}out ] ; then
          rm ${p}out
        fi
        grep ${base}$dy$hh ${p}index | wgrib -i ${pole}.$base.grb -nh -o ${p}out
        if [ -s ${p}out ] ; then
          ./${p}thickxpm ${p}out ${p}out.land ${p}t${base}${dy}${hh}.xpm
          xpmtogif ${p}t${base}${dy}${hh}.xpm
          rm ${p}t${base}${dy}${hh}.xpm
        fi
      done
    done
  
    ls -1 ${p}t${base}*.gif > beta
    whirlgif -o ${p}thick.$base.gif -i beta
    rm ${p}t${base}*.gif  

  done
done
