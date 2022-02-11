#!/bin/bash
#set -e
set -x

yy=00
mo=01
dy=01
while [ $yy -lt 1 ]
do
  while [ $mo -le 2 ]
  do
    if [ ! -d $yy$mo ] ; then
      mkdir $yy$mo
      mv *${yy}${mo}* $yy$mo
    fi
    cd $yy$mo

    while [ $dy -le 31 ] 
    do
      for n in n3s s3s
      do
        #fn=$yy$mo$dy.$n
        #if [ -f $fn ] ; then
        #  d=`echo $fn | cut -f1 -d\.`
        #  mv $fn ${n}smi.$d
        #fi
        if [ -f ${n}smi.$yy$mo$dy ] ; then
          mv ${n}smi.$yy$mo$dy ${n}smi.20$yy$mo$dy
        fi
      done

      dy=`expr $dy + 1`
      if [ $dy -lt 10 ] ; then
        dy=0$dy
      fi
    done
    dy=01

    mo=`expr $mo + 1`
    if [ $mo -lt 10 ] ; then
      mo=0$mo
    fi

    cd /usr1/data/3d/satellites/ncep.old
  done
  mo=01
  dy=01
  yy=`expr $yy + 1`
done
