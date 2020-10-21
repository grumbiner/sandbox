#@ job_type=serial
#@ output=/nfsuser/g01/wx21rg/icemodel/running/octout
#@ error=/nfsuser/g01/wx21rg/icemodel/running/octerrout
#@ class=1
#@ wall_clock_limit=00:20:00
#@ queue    

#!/bin/ksh
base=200101
dy=01
CC=20
export CC base

#-------------------------------
set -x
cd ~/icemodel/control 

export ICEASSIM=.FALSE.
export polelist='north '

while [ $dy -le 31 ] 
do
  time /usr1/rmg3/icemodel/execs/forecast.sh ${base}$dy  > ${base}$dy 2> ${base}${dy}.2
  dy=`expr $dy + 1`
  if [ $dy -lt 10 ] ; then
    dy=0$dy
  fi
done
nohup time ./feb.sh &
