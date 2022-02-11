#!/bin/sh
#@ job_type=serial
#@ output=/u/wx21rg/newamsr.few.out
#@ error=/u/wx21rg/newamsr.few.err
#@ class=1
#@ wall_clock_limit=00:30:00
#@ account_no = RTO_T2O
#@ resources = ConsumableCpus(1) ConsumableMemory(900 MB)
#@ queue

set -x

SYSTEM=IBM
  #RUNDIR=/stmp/wd21rg/rfc2b.2011
  RUNDIR=/u/wx21rg/noscrub/rfc2b.2011
  EXDIR=/u/wx21rg/rgdev/ice1/rfc2.2011.execs/
  FIX=/u/wx21rg/rgdev/ice1/rfc2.2011.fix/
  OUTDIR=/u/wx21rg/noscrub/rfc2b.2011/

if [ $SYSTEM = "IBM" ] ; then
  #Directives for the IBM CCS:
  DTGDIR=/u/wx21rg/bin/
  #DCOMROOT=/dcom/
  DCOMROOT=/u/wx21rg/noscrub/amsre/2yr/
  TANKFARM=/u/wx21rg/noscrub/amsre/2yr/us007003
  TANK=/u/wx21rg/noscrub/amsre/2yr/us007003
  SSMIDIR=/u/wx21rg/noscrub/ssmi85/
  AMSRDIR=/u/wx21rg/noscrub/amsre/
  SSTDIR=/u/wx21rg/noscrub/sst/
  ALTSST=/u/wx21rg/noscrub/sst/
  PATH=$PATH:/nwprod/util/exec/:/u/wx21rg/bin/:/usrx/local/imajik/bin/
  ASSESS=hpmcount
fi

export DCOMROOT TANKFARM TANK RUNDIR EXDIR FIX OUTDIR SSMIDIR AMSRDIR SSTDIR ALTSST 

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


#Perform a posteriori filtering -- as was done for CFSRR -- 21 April 2011
#  Rename above files from umasknorth/south to initnorth/south so that output here
#  can be the umask names, and rest of script is unchanged:
  if [ ! -f seaice_posteriori_5min ] ; then
    cp $FIX/seaice_posteriori_5min .
  fi
  $ASSESS $EXDIR/seaice_posteriori_5min.x seaice_posteriori_5min \
      umasknorth12.$tag4 umasksouth12.$tag4 \
      posteriori_north.$tag4 posteriori_south.$tag4 > postout.$tag4



#New -- monitor files against yesterday, and product an ice edge point
#   set for use by drift model
  $ASSESS $EXDIR/seaice_monitor_c12th  fill5min.$tag4 ${OUTDIR}/fill5min.$tagm  \
                           $FIX/seaice_gland5min seaice_delta_$tag4.xpm \
                           seaice_monitor_${tag4}.kml \
                           > seaice_monitor_${tag4}.txt
  $ASSESS $EXDIR/seaice_edge fill5min.$tag4 $FIX/seaice_gland5min \
                     seaice_edge_${tag4}.kml > seaice_edge_${tag4}.txt

