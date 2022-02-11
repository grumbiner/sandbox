#!/bin/sh
#@ job_type=serial
#@ output=/nfsuser/g01/wx21rg/ssmi/batchout
#@ error=/nfsuser/g01/wx21rg/ssmi/errout
#@ class=1
#@ wall_clock_limit=00:20:00
#@ queue

#Directives for the IBM CCS:
DTGDIR=/nfsuser/g01/marine/local/bin/
RUNDIR=/emc1/ice/ice.analy.high
TMPDIR=$RUNDIR
EXDIR=/nfsuser/g01/wx21rg/hires/execs
FIX=/nfsuser/g01/wx21rg/hires/fix
DATDIR=/nfsuser/g01/wx21rg/hires/fix
OUTDIR=/emc1/ice/ice.analy.high
SSMIDIR=/emc1/ice/ssmi85
SSTDIR=/emc1/ice/sst.high
OLDDIR=/emc1/ice/ice.old.high
ADIR1=$OUTDIR
PATH=$PATH:/nwprod/util/exec/:/nfsuser/g01/marine/local/bin:/nfsuser/g01/marine/local/bin/netpbm

export RUNDIR TMPDIR EXDIR DATDIR OUTDIR SSMIDIR SSTDIR OLDDIR ADIR1 UNBLKDIR

# ---------- Should need no changes below here ------------------
set -xe

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

#Make up the grib files for the polar stereographic data
echo $tag4 | cut -c1-2  > psin
echo $tag4 | cut -c3-4  >> psin
echo $tag4 | cut -c5-6  >> psin
echo $tag4 | cut -c7-8  >> psin

#Engribbing of PS grids:
  for pole in north south north12 south12
  do
    if [ -f fort.11 ] ; then
       rm fort.11  
    fi
    if [ -f fort.51 ] ; then
       rm fort.51
    fi
    if [ ! -s $OUTDIR/${pole}psg.$tag4 ] ; then
      ln umask${pole}.$tag4 fort.11
      hpmcount ${EXDIR}/seaice_psg${pole} < psin
      if [ -f fort.51 ] ; then
        mv fort.51 ${pole}psg.$tag4
      fi
      if [ -f fort.52 ] ; then
        mv fort.52 wmo${pole}psg.j990l
      fi
    fi
  done

##########################################################
