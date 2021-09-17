#!/bin/ksh
base=200010
dy=01
CC=20
export CC base

#-------------------------------
set -x
cd ~/icemodel/control 

export ICEASSIM=.FALSE.
export polelist='north '

tag=${base}$dy
while [ $tag -le 20010930 ] 
do
  time /usr1/rmg3/icemodel/execs/forecast.sh ${base}$dy  > ${base}$dy 2> ${base}${dy}.2

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
  base=`echo $tag | cut -c1-6`
  dy=`echo $tag | cut -c7-8`  

done
