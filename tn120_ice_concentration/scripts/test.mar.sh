#!/bin/sh
#@ job_type=serial
#@ output=/nfsuser/g01/wx21rg/ssmi/ssmi/batchout
#@ error=/nfsuser/g01/wx21rg/ssmi/ssmi/ssmierrout
#@ class=1
#@ wall_clock_limit=05:00:00
#@ requirements = Feature == "beta"
#@ queue     

set -xe
cd /nfsuser/g01/wx21rg/ssmi/ssmi
for clear in fort.14 fort.51
do
  if [ -f $clear ] ; then
    rm $clear
  fi
done

tag=20000225

while [ $tag -le 20000331 ]
do
  #Process the bufr output into something readable
  if [ ! -f /gpfs/emc/marine/ice/ssmi85/process1.$tag ] ; then
    ln -sf /gpfs/emc/marine/ice/ssmi85/ssmi85.$tag fort.14
    time ./ssmibufr.x
    rm fort.14
    cp fort.51 /gpfs/emc/marine/ice/ssmi85/process1.$tag
  else
    ln -sf /gpfs/emc/marine/ice/ssmi85/process1.$tag fort.51
  fi 


  #Construct the test algorithms
  for prog in global north south
  do
    time ./$prog fort.51 > ssout.$prog.$tag
    for p in 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 
    do
      if [ -f parm$p ] ; then
        mv parm$p parm${p}.$prog.$tag
      fi
    done
  done
  
  mv sconc.out sconc.$tag
  mv global.out global.$tag
  mv nconc.out nconc.$tag
  rm fort.51

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done

llsubmit /nfsuser/g01/wx21rg/ssmi/ssmi/test.apr.sh
