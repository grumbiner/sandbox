#!/bin/sh
#@ job_type=serial
#@ output=/u/wx21rg/lakeice.out
#@ error=/u/wx21rg/lakeice.err
#@ class=1
#@ wall_clock_limit=00:30:00
#@ account_no = RTO_T2O
#@ resources = ConsumableCpus(1) ConsumableMemory(900 MB)
#@ queue

#set -x

SYSTEM=IBM

if [ $SYSTEM = "IBM" ] ; then
  #Directives for the IBM CCS:
  DTGDIR=/u/wx21rg/bin/
  RUNDIR=/ptmp/wd21rg/lakeice
  TMPDIR=$RUNDIR
  EXDIR=/u/wx21rg/rgdev/satellites/ssmi
  FIX=/u/wx21rg/save/ssmi/fix
  OUTDIR=/u/wx21rg/noscrub/lakeice/
  SSMIDIR=/u/wx21rg/noscrub/ssmi85/
  SSTDIR=/com/gfs/prod/sst.
  PATH=$PATH:/nwprod/util/exec/:/u/wx21rg/bin/:/usrx/local/imajik/bin/
  ASSESS=time
fi

export RUNDIR TMPDIR EXDIR FIX OUTDIR SSMIDIR SSTDIR 

# ---------- Should need no changes below here ------------------
if [ ! -d $RUNDIR ] ; then
  echo about to try to make directory $RUNDIR
  mkdir -p $RUNDIR
fi
cd $RUNDIR
if [ `pwd` != $RUNDIR ] ; then
  echo Could not change directory to $RUNDIR  Failing!!!
  exit
fi

#Script variant to catch up for a specified date.  Rebuild what needs
# to be rebuilt, and run from there.  
#If year and date tag are given use those, else rerun today 
if [ $# -ge 2 ] ; then
  yy=$1
  rest=$2
fi
yy=${yy:-`date +"%Y"`}
rest=${rest:-`date +"%m%d"`}  
tag4=${yy}$rest
tagm=`expr $tag4 - 1`
tagm=`${DTGDIR}/dtgfix3 $tagm`
echo $tagm
#dummy variable
jday=`date +"%j" `


#---------------------------------------------


echo starting the scan program
# Now start new processing
$EXDIR/scan fort.51 fred 
echo done with scan
