#!/bin/sh
#@ job_type=serial
#@ output=/nfsuser/g01/wx21rg/ssmi/ssmi/batchout
#@ error=/nfsuser/g01/wx21rg/ssmi/ssmi/ssmierrout
#@ class=1
#@ wall_clock_limit=06:00:00
#@ requirements = Feature == "beta"
#@ queue     

cd /gpfs/emc/marine/ice/ssmi85/
cp /nfsuser/g01/wx21rg/ssmi/ssmi/ssmibufr.x .
cp /nfsuser/g01/wx21rg/ssmi/ssmi/toprocess1 .
rm fort.14 fort.51


tag=20000225

while [ $tag -le 20000331 ]
do
  #Process the bufr output into something readable
  ln -sf /gpfs/emc/marine/ice/ssmi85/ssmi85.$tag fort.14
  time ./ssmibufr.x
  rm fort.14

  ./toprocess1
  mv process1.out process1.$tag
  rm fort.51

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done

llsubmit /nfsuser/g01/wx21rg/ssmi/ssmi/source/proc1.apr.sh
