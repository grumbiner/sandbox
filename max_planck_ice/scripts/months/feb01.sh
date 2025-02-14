#@ job_type=serial
#@ output=/nfsuser/g01/wx21rg/icemodel/running/febout
#@ error=/nfsuser/g01/wx21rg/icemodel/running/feberrout
#@ class=1
#@ wall_clock_limit=09:30:00
#@ queue    

#!/bin/ksh
base=200102

dy=01
CC=20
export CC base

set -x
cd ~/nfsuser/icemodel/running

count=0
while [ $dy -le 28 ] 
do
  count=`expr $count + 1`
  time ./ibm.sh ${base}$dy  > ${base}$dy 2> ${base}${dy}.2
  #Following is for linux
  #if [ $count -ge 4 ] ; then
  #  sync
  #fi
  dy=`expr $dy + 1`
  if [ $dy -lt 10 ] ; then
    dy=0$dy
  fi
done
llsubmit /nfsuser/g01/wx21rg/icemodel/running/mar01.sh
