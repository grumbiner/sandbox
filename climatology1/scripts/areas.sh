#!/bin/sh

EXECdir=~/svn/ice.climo/exec/
if [ ! -f seaice_newland ] ; then
  cp ~/concentration.svn/fix/seaice_newland .
fi

#-------------------------------------------------------
j=0
tag=19810101
while [ $tag -lt 20101231 ]
do
  if [ ! -s ice.$tag ] ; then
    echo missing ice.$tag
  fi

  x=`${EXECdir}/area seaice_newland ice.$tag`
  echo $j $x >> areas

  j=`expr $j + 1`
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done  
