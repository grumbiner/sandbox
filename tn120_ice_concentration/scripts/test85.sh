#!/bin/sh
#@ job_type=serial
#@ output=/nfsuser/g01/wx21rg/ssmi/ssmi/batchout
#@ error=/nfsuser/g01/wx21rg/ssmi/ssmi/ssmierrout
#@ class=1
#@ wall_clock_limit=04:00:00
#@ requirements = Feature == "beta"
#@ queue     

set -x
cd /nfsuser/g01/wx21rg/ssmi/ssmi
execute=./south

tag=20000225

while [ $tag -le 20000225 ]
do
  #Process the bufr output into something readable
  ln -sf /gpfs/emc/marine/ice/ssmi85/ssmi85.$tag fort.14
  time ./ssmibufr.x
  rm fort.14

  #Construct the test algorithms
  time $execute fort.51 > ssout.$tag
  #find . -name 'parm??' -exec mv {} {}.$tag \;
  for p in 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 
  do
    if [ -f parm$p ] ; then
      mv parm$p parm${p}.$tag
    fi
  done
  

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done

