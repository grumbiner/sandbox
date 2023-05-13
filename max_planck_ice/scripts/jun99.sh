#@ job_type=serial
#@ output=/nfsuser/g01/wx21rg/icemodel/running/marout
#@ error=/nfsuser/g01/wx21rg/icemodel/running/marerrout
#@ class=1
#@ wall_clock_limit=09:10:00
#@ requirements = Feature == "beta"
#@ queue    

#!/bin/ksh
base=9906

dy=01
count=0

CC=19
export CC base

set -x
while [ $dy -le 30 ] 
do
  count=`expr $count + 1`
  time ./ibm.sh ${base}$dy  > ${base}$dy 2> ${base}${dy}.2
  if [ $count -ge 4 ] ; then
    sync
  fi
  dy=`expr $dy + 1`
  if [ $dy -lt 10 ] ; then
    dy=0$dy
  fi
done
  
llsubmit /nfsuser/g01/wx21rg/icemodel/running/jul99.sh
