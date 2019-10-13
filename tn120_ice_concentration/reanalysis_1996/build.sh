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

SSTFILT=275.15

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

#Set up the sst indices:
for y in 5 6 7 8 9 
do
  if [ ! -s sst.index.199$y ] ; then
    wgrib sst.199$y > sst.index.199$y
  fi
done


#set -x
#First pass
#From 'now' to enddate, construct as much of the data suite as possible
#Since this stage does not (cannot) interpolate across missing days,
#  need a second pass. 
while [ $tag -le $enddate ]
do
  base=`echo $tag | cut -c1-4`
  yy=`echo $tag | cut -c1-2`

  if [ ! -s nindex.$base ] ; then
    wgrib npsg.${base} > nindex.$base
  fi
  if [ ! -s sindex.$base ] ; then
    wgrib spsg.${base} > sindex.$base
  fi

#Get ice concentrations for given dates if they dont already exist
# as nonzero size files -- do only if we haven't already constructed
# a latlon file.
  if [ ! -s $OUTDIR/latlon.$tag ] ; then
    if [ ! -s nout.$tag ] ; then
      echo grepping for $tag
      grep "d=$tag" nindex.$base | wgrib -i npsg.${base} -o nout.$tag -nh
    fi
    if [ ! -s sout.$tag ] ; then
      grep "d=$tag" sindex.$base | wgrib -i spsg.${base} -o sout.$tag -nh
    fi
    if [ ! -s nout.$tag ] ; then
      echo failed on north $tag >> bad.dates
    fi
    if [ ! -s sout.$tag ] ; then
      echo failed on south $tag >> bad.dates
    fi
  # Get the sst's to filter with.
    grep $tag sst.index.19$yy | wgrib -i sst.19$yy -o sst.tmp -nh
    if [ -s sst.tmp ] ; then
      mv sst.tmp sst
    fi
  fi #end of prepping files to construct a latlon with

# Now set about filtering the data, if there isn't already a latlon file,
#   make one
  if [ ! -s $OUTDIR/latlon.$tag ] ; then
    if [ ! -s nout.$tag -o ! -s sout.$tag ] ; then
      echo cannot produce latlon for $tag >> bad.dates
     else
      $EXDIR/filt3 sst nout.$tag sout.$tag $OUTDIR/latlon.$tag $OUTDIR/nps.$tag $OUTDIR/sps.$tag $CDIR/half.new.binary $SSTFILT $CDIR/nland.map $CDIR/sland.map
      rm [ns]out.$tag $OUTDIR/[ns]ps.$tag
    fi
  fi

#And then fill in the grids if the previous day is nonzero size
  if [ -s $OUTDIR/age.$tagm -a -s $OUTDIR/fill.$tagm ] ; then
    echo Filling $tag
    $EXDIR/fill $OUTDIR/fill.$tagm $OUTDIR/latlon.$tag $OUTDIR/age.$tagm $OUTDIR/age.$tag $OUTDIR/fill.$tag
  fi
    
  tagm=$tag
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
  yy=`echo $tag | cut -c1-2`

done     
#This ends phase 1.  All latlon files are generated for which there is
#  data on the corresponding polar stereographic grid.  Once a day was 
#  reached, however, for which there was no ps grid, the filled files
#  could not be constructed.  Hence need phase 2.



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
