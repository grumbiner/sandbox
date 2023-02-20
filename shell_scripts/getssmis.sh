#!/bin/sh
#Script to retrieve and archive the full-res SSM/IS
#Robert Grumbine 29 February 2000
#derived from ssmi script
#Robert Grumbine 5 October 2011
# Conditional execution so as to permit multi-site invocation

set -x

export RUNDIR=/stmp/wx21rg/ssmis
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

if [ ! -s /u/wx21rg/noscrub/ssmisu/ssmisu.$tag ] ; then
  time  /nwprod/ush/dumpjb ${tag}00 12 ssmisu
  mv ssmisu.ibm /u/wx21rg/noscrub/ssmisu/ssmisu.$tag
fi
