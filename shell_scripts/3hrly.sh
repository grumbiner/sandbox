#!/bin/sh

#htar -tvf /hpssprod/runhistory/rh2002/200206/20020630/com_fnl_prod_fnl.2002063000.tar
#exit

yy=2002
for mm in 07 08 09 10
do
  dd=00
  while [ $dy -lt 31 ]
  do
    dd=`expr $dd + 1`
    if [ $dd -lt 10 ] ; then
      dd=0$dd
    fi

    for hh in 00 06 12 18
    do

      htar -tvf /hpssprod/runhistory/rh${yy}/${yy}${mm}/${yy}${mm}${dd}/com_fnl_prod_fnl.${yy}${mm}${dd}${hh}.tar ./gdas1.t${hh}z.sfluxgrbf00 ./gdas1.t${hh}z.sfluxgrbf03 ./gdas1.t${hh}z.sfluxgrbf06

    done

  done
done
