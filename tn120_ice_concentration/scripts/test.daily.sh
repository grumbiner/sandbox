#!/bin/sh
#@ job_type=serial
#@ output=/nfsuser/g01/wx21rg/ssmi/ssmi/batchout
#@ error=/nfsuser/g01/wx21rg/ssmi/ssmi/ssmierrout
#@ class=1
#@ wall_clock_limit=00:10:00
#@ requirements = Feature == "beta"
#@ queue     

set -x
cd /nfsuser/g01/wx21rg/ssmi/ssmi
#rm fort.51 fort.14

tag=`date +"%Y%m%d"`
tag=20000227

#Process the bufr output into something readable
##if [ -f /gpfs/emc/marine/ice/ssmi85/process1.$tag ] ; then
##  ln -sf /gpfs/emc/marine/ice/ssmi85/process1.$tag fort.51
##else
  ln -sf /gpfs/emc/marine/ice/ssmi85/ssmi85.$tag fort.14
  time ./ssmibufr.x
  rm fort.14
##fi

#Construct the test algorithms
for prog in north south global 
#for prog in south 
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
mv nconc.out nconc.$tag
mv global.out gconc.$tag
#rm fort.51

