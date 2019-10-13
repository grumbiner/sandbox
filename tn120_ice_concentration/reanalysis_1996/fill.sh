#!/bin/bash

#Script to control getting sea ice concentrations converted to lat-long
#  grids and filtered appropriately.
#First version 12/98.
#Revised 15 September 1999
#Robert Grumbine

CDIR=/home/rmg3/ssmi/land.recomp/
EXDIR=/home/rmg3/ssmi/recompute/
OUTDIR=/data/meteo/global.new
PROCDIR=/mnt/disk3/

tag=950916
tagm=950915
enddate=990831
#enddate=950920
start=$tag
export CDIR EXDIR OUTDIR PROCDIR tag tagm enddate start

SSTFILT=276.15

#Should be no changes below here ---------------

#Check for required directories.
if [ ! -d $EXDIR ] ; then
  echo cannot find executables directory.  Aborting.
  exit 1
fi
if [ ! -d $CDIR ] ; then
  echo cannot find reference files directory.  Aborting
  exit 2
fi
if [ ! -d $PROCDIR ] ; then
  echo cannot find processing directory which is to contain the ice and sst data
  exit 3
fi
if [ ! -d $OUTDIR ] ; then
  mkdir -p $OUTDIR
fi

cd $PROCDIR
#See if we have files to initialize with
if [ ! -f $OUTDIR/fill.$tagm -o ! -f $OUTDIR/age.$tagm ] ; then
  echo Do not have age and fill files for the first 'yesterday'
  exit -1
fi


#Phase 2: Loop through period and see if there are missing latlon or fill
#  files.  If latlon is missing, interpolate.  If fill is missing, see if
#  we now have a latlon and proceed.  If still no latlon, complain loudly
#  and quit.
tag=$start
tagm=`expr $tag - 1`
tagm=`dtgfix3 $tagm`
while [ $tag -le $enddate ] 
do

  #Construct latlon files if needed:
  k=0
  tmp=$tag
  if [ ! -s $OUTDIR/latlon.$tmp ] ; then
    while [ ! -s $OUTDIR/latlon.$tmp ] 
    do 
      k=`expr $k + 1`
      tmp=`expr $tmp + 1`
      tmp=`dtgfix3 $tmp`
    done
  $EXDIR/time_interp $OUTDIR/latlon.$tagm $OUTDIR/latlon.$tmp $k
    j=0
    tmp=$tag
    while [ $j -lt $k ]
    do
      mv out.0$j $OUTDIR/latlon.$tmp
      j=`expr $j + 1`
      tmp=`expr $tmp + 1`
      tmp=`dtgfix3 $tmp`
    done
  fi
      

  #And then fill in the grids if the previous day is nonzero size
  if [ -s $OUTDIR/age.$tagm -a -s $OUTDIR/fill.$tagm -a \
      \( ! -s $OUTDIR/age.$tag -a ! -s $OUTDIR/fill.$tag \) ] ; then
    echo Filling $tag
    $EXDIR/fill $OUTDIR/fill.$tagm $OUTDIR/latlon.$tag $OUTDIR/age.$tagm $OUTDIR/age.$tag $OUTDIR/fill.$tag
  fi
    
  tagm=$tag
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`

done
