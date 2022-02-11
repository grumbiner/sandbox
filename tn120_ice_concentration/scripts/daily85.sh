#!/bin/sh
#@ job_type=serial
#@ output=/nfsuser/g01/wx21rg/ssmi/ssmi/dailyout
#@ error=/nfsuser/g01/wx21rg/ssmi/ssmi/dailyerrout
#@ class=1
#@ wall_clock_limit=00:20:00
#@ queue     

set -x

TMPDIR=/gpfstmp/wx21rg/hires
if [ ! -d $TMPDIR ] ; then
  mkdir -p $TMPDIR
fi
cd $TMPDIR
EXDIR=/nfsuser/g01/wx21rg/ssmi/ssmi
ARCDIR=/emc1/ice
export ARCDIR EXDIR TMPDIR
#export TANK=/ptmp/wx20kz/tankfarm
PATH=$PATH:/nfsuser/g01/marine/local/bin:/nfsuser/g01/marine/local/bin/netpbm

#
#Should not need any changes below this point -----------------------
#
for clear in fort.51 fort.14
do
  if [ -f $clear ] ; then
    rm $clear
  fi
done 

tag=`date +"%Y%m%d"`

for execs in ssmibufr.x north south global gland.new sland.new nland.new globalxpm northxpm southxpm
do
  cp $EXDIR/$execs .
done

#Get and process the bufr tank into something readable
if [ ! -f $ARCDIR/ssmi85/ssmi85.$tag ] ; then
  time   /nwprod/ush/dumpjb ${tag}00 12 ssmit
  ln ssmit.ibm fort.14
else
  ln -s $ARCDIR/ssmi85/ssmi85.$tag fort.14
fi
if [ ! -f fort.14 ] ; then
  echo failed to construct an ssmi input file!!
  exit 1
fi
time ./ssmibufr.x
rm fort.14

#Construct the test algorithms
for prog in north south global 
do
  p=`echo $prog | cut -c1`
  time ./$prog fort.51 ${p}land.new ${p}conc.out > ssout.$prog.$tag
  for p in 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 
  do
    if [ -f parm$p ] ; then
      mv parm$p parm${p}.$prog.$tag
    fi
  done
done

#Put the output in a usable location:
for pole in north south global 
do
  p=`echo $pole | cut -c1`
  if [ -f parm13.$pole.$tag ] ; then
    ./${pole}xpm parm13.$pole.$tag ${p}land.new ${pole}new.xpm
    xpmtogif ${pole}new.xpm
    rcp ${pole}new.gif rmg3@polar:/data/WWW/seaice/expt.analy/
    rcp ${pole}new.gif rmg3@polar:/data/WWW/seaice/expt.analy/history/${pole}new.$tag.gif
  fi
  ./${pole}xpm ${p}conc.out ${p}land.new ${pole}ref.xpm
  xpmtogif ${pole}ref.xpm
  rcp ${pole}ref.gif rmg3@polar:/data/WWW/seaice/expt.analy/
  rcp ${pole}ref.gif rmg3@polar:/data/WWW/seaice/expt.analy/history/${pole}ref.$tag.gif
done

mv sconc.out sconc.$tag
mv nconc.out nconc.$tag
mv global.out gconc.$tag
mv *$tag $EXDIR
