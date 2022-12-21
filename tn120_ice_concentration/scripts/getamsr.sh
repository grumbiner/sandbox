#!/bin/sh
#Script to retrieve and archive the full-res 85 GHz SSMI
#Robert Grumbine 29 February 2000
# Conditional execution so as to permit multi-site invocation

#set -x

export RUNDIR=/ptmp/wx21rg/amsrt/
export DCOMROOT=/dcom

if [ ! -d $RUNDIR ] ; then
  mkdir -p $RUNDIR
fi
cd $RUNDIR

tag=20110325

export TMPDIR=`pwd`

hr=15
while [ $hr -le 40 ]
do
  if [ ! -s /u/wx21rg/noscrub/amsre/amsre.$tag ] ; then
    time  /nwprod/ush/dumpjb ${tag}00 12 amsre
    mv amsre.ibm /u/wx21rg/noscrub/datatest/amsre.$tag.$hr
  fi
 
  hr=`expr $hr + 1`
  sleep 3500
done
