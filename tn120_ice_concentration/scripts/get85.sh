#!/bin/sh
#Script to retrieve and archive the full-res 85 GHz SSMI
#Robert Grumbine 29 February 2000
# Conditional execution so as to permit multi-site invocation

set -x

export RUNDIR=/stmp/wx21rg/ssmi
export DCOMROOT=/dcom

if [ ! -d $RUNDIR ] ; then
  mkdir -p $RUNDIR
fi
cd $RUNDIR

if [ $# -eq 1 ] ; then
  tag=$1
else
  tag=`date +"%Y%m%d"`
fi

export TMPDIR=`pwd`

if [ ! -s /u/wx21rg/noscrub/ssmi85/ssmi85.$tag ] ; then
  time  /nwprod/ush/dumpjb ${tag}00 12 ssmit
  mv ssmit.ibm /u/wx21rg/noscrub/ssmi85/ssmi85.$tag
fi
