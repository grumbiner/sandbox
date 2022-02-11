#!/bin/sh

set -x

PATH=$PATH:/nfsuser/g01/marine/local/bin:/nwprod/util/exec
cd /ptmp/wx21rg/
if [ ! -f ssmibufr.x ] ; then
  cp /nfsuser/g01/wx21rg/test/bufr/ssmibufr.x .
fi

tag=20021110

while [ $tag -le 20030131 ]
do

  #Process the bufr output into something readable
  if [ ! -f ssmi85.$tag -a -f /emc1/ice/ssmi85/ssmi85.$tag ] ; then
    rcp hanfs2:/emc1/ice/ssmi85/ssmi85.$tag .
    ln -sf ssmi85.$tag fort.14 
    time ssmibufr.x
    mv fort.51 ssmiflat.$tag
  else 
    if [ -f ssmi85.$tag ] ; then
      ln -sf ssmi85.$tag fort.14 
      time ./ssmibufr.x
      mv fort.51 ssmiflat.$tag
     else
      echo missing ssmi85.$tag
     fi
  fi

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
