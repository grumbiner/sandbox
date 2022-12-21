#!/bin/sh

OUTDIR=/marine/noscrub/wx21rg/amsr
RUNDIR=/ptmp/wx21rg/amsr
EXDIR=/marine/save/rgdev/satellites/amsr

#set -x
#---------------- Should need no changes below here
if [ ! -d OUTDIR ] ; then
  mkdir -p $OUTDIR
fi
if [ ! -d RUNDIR ] ; then
  mkdir -p $RUNDIR
fi

cd $RUNDIR

if [ $# -ne 1 ] ; then
  tag=`date +"%Y%m%d"`
else
  tag=$1
fi

export XLFRTEOPTS="unit_vars=yes"


  #export XLFUNIT_11=avcl18.ibm
  #export XLFUNIT_11=/dcom/us007003/$tag/b012/xx012
  #export XLFUNIT_11=/dcom/us007003/$tag/b012/xx021 -- radiances
  #export XLFUNIT_11=/dcom/us007003/$tag/b021/xx053 -- avcs18
  #export XLFUNIT_11=/dcom/us007003/$tag/b021/xx054 -- avcl18

  if [ ! -f amsre.ibm ] ; then
    /nwprod/ush/dumpjb  ${tag}00 12 amsre
  fi

  ls -l
  export XLFUNIT_11=amsre.ibm
  if [ ! -s $OUTDIR/amsr.$tag ] ; then
    if [ -f $XLFUNIT_11 ] ; then
      echo working on $tag
      $EXDIR/bufr_readamsr > rg.out.$tag
      #mv fort.52 amsr.out
    fi
 fi
