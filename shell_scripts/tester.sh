#!/bin/sh
#@ job_type=serial
#@ output=/nfsuser/g01/wx21rg/hires/batchout
#@ error=/nfsuser/g01/wx21rg/hires/errout
#@ class=1
#@ wall_clock_limit=00:10:00
#@ queue

SYSTEM=STOMMEL

set -xe

  #Directives for Stommel:
  DTGDIR=/usr1/home/wd21rg/bin/
  RUNDIR=/usr1/dataflow/ice/tmp3
  OUTDIR=/usr1/dataflow/ice/tmp3
  TMPDIR=$RUNDIR
  EXDIR=/usr1/home/wd21rg/newer.ice/execs
  DATDIR=/usr1/home/wd21rg/fix
  FIX=/usr1/home/wd21rg/fix
  SSMIDIR=/usr1/dataflow/ssmi85
  SSTDIR=/usr1/dataflow/sst
  OLDDIR=/usr1/dataflow/ice.old
  UNBLKDIR=/usr1/dataflow/unblock
  ADIR1=$OUTDIR
  ASSESS=

export RUNDIR TMPDIR EXDIR FIX DATDIR OUTDIR SSMIDIR SSTDIR OLDDIR ADIR1 UNBLKDIR

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
#ensure that we don't have local debris:
rm -rf *

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

#If there isn't an ssmi file, construct one
#don't bother with ssmi tank if we already have an n3ssmi file:
if [ ! -s $OUTDIR/n3ssmi.$tag4 ] ; then

  if [ ! -s $SSMIDIR/ssmi85.$tag4 ] ; then 
    #IBM time /nwprod/ush/dumpjb ${yy}${rest}00 12 ssmit
    echo Would have had to construct ssmi85.$tag4 on stommel, failing
    exit
  else
    #IBM cp $SSMIDIR/ssmi85.$tag4 ssmit.ibm
    ln -sf $SSMIDIR/ssmi85.$tag4 ssmit.ibm
  fi

# From stommel version, look to unblock directory:
  if [ -s $UNBLKDIR/unblock.$tag4 ] ; then
    ln $UNBLKDIR/unblock.$tag4 fort.51
  else
    #Must rerun this part (?)
    if [ -f fort.51 ] ; then
      rm fort.51
    fi
    if [ -f fort.14 ] ; then
      rm fort.14
    fi
    ln ssmit.ibm fort.14
    $ASSESS $EXDIR/seaice_ssmibufr 
  fi

fi

echo fort.51 > delta
if [ ! -f TBccant.tab ] ; then
  cp $FIX/*.tab .
fi 

/usr1/home/wd21rg/newer.ice/tester delta $FIX/seaice_nland127.map $FIX/seaice_sland127.map \
       n3ssmi.$tag4 s3ssmi.${tag4} \
       umasknorth12.$tag4 umasksouth12.$tag4 \
       $jday $yy 246 > v.$tag4
#Construct 25.4 km versions:  NEW:
$EXDIR/seaice_north_reduce umasknorth12.$tag4 umasknorth.$tag4 \
                        $FIX/seaice_nland127.map $FIX/seaice_nland.map
$EXDIR/seaice_south_reduce umasksouth12.$tag4 umasksouth.$tag4 \
                        $FIX/seaice_sland127.map $FIX/seaice_sland.map

#Transfer files to output directory:
#rm unblock.$tag4
