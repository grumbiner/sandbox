#!/bin/bash

#Script to control getting sea ice concentrations converted to lat-long
#  grids and filtered appropriately.
#First version 12/98.
#Revised 15 September 1999
#Robert Grumbine
#Variant for old filter program (incorrect mapping)

#CDIR    contains the land masks halfdeg.map, nland.map, sland.map
#EXDIR   contains executables filt3, fill, time_interp
#OUTDIR  is destination for output
#PROCDIR is directory with sst and sea ice grib files

CDIR=~/land.os/km25/
EXDIR=~/allice/recompute/
OUTDIR=~/disk1/concentrations/global.old/
PROCDIR=~/disk1/concentrations/reanalysis/

tag=011208
tagm=011207
enddate=011210
start=$tag

export CDIR EXDIR OUTDIR PROCDIR tag tagm enddate start

SSTFILTK=275.15
SSTFILTC=2.0

#Should be no changes below here ---------------

#Check for required directories.
if [ ! -d $EXDIR ] ; then
  echo cannot find executables directory $EXDIR   Aborting.
  #Ensure that the executables are available:
  cd $EXDIR
  make
  make
  exit 1
fi
if [ ! -d $CDIR ] ; then
  echo cannot find reference files directory $CDIR  Aborting
  exit 2
fi
if [ ! -d $PROCDIR ] ; then
  echo cannot find $PROCDIR which is to contain the ice and sst data
  exit 3
fi
if [ ! -d $OUTDIR ] ; then
  mkdir -p $OUTDIR
  if [ ! -d $OUTDIR ] ; then
    echo Failed to construct $OUTDIR for output, aborting
    exit 4
  fi
fi

cd $PROCDIR
#See if we have files to initialize with
if [ ! -f $OUTDIR/fill.$tagm -o ! -f $OUTDIR/age.$tagm ] ; then
  echo Do not have age and fill files for the first 'yesterday'
  exit -1
fi

#Set up the sst indices:
for y in 5 6 7 8 9 10 11
do
  year=`expr 1990 + $y`
  if [ ! -s sst.index.$year ] ; then
    if [ -s sst.$year ] ; then
      wgrib sst.$year > sst.index.$year
    else
      echo failed to open an sst file for $year
      exit -2
    fi
  fi
done
if [ ! -s rtg_sst.index.2001 ] ; then
  wgrib rtg_sst.2001 > rtg_sst.index.2001
fi

echo First pass
#From 'now' to enddate, construct as much of the data suite as possible
#Since this stage does not (cannot) interpolate across missing days,
#  need a second pass. 
while [ $tag -le $enddate ]
do
  base=`echo $tag | cut -c1-4`
  yy=`echo $tag | cut -c1-2`

  #Get the hemispheric archive files if needed
  if [ ! -f npsg.$base ] ; then
    if [ $yy -lt 50 ] ; then
      if [ -f npsg.20$base ] ; then
        mv npsg.20$base npsg.$base
        mv spsg.20$base spsg.$base
       else
        rcp polar:/home/ftp/pub/ice/archive/npsg.20${base}.Z .
        rcp polar:/home/ftp/pub/ice/archive/spsg.20${base}.Z .
        uncompress *.Z
        mv npsg.20$base npsg.$base
        mv spsg.20$base spsg.$base
      fi
    else
      rcp polar:/home/ftp/pub/ice/archive/npsg.${base}.Z .
      rcp polar:/home/ftp/pub/ice/archive/spsg.${base}.Z .
      uncompress *.Z
    fi
  fi

  #Construct grib indices if needed
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
      grep "$tag" nindex.$base | wgrib -i npsg.${base} -o nout.$tag -nh
    fi
    if [ ! -s sout.$tag ] ; then
      grep "$tag" sindex.$base | wgrib -i spsg.${base} -o sout.$tag -nh
    fi
    if [ ! -s nout.$tag ] ; then
      echo failed on north $tag >> bad.dates
    fi
    if [ ! -s sout.$tag ] ; then
      echo failed on south $tag >> bad.dates
    fi
  # Get the sst's to filter with.
    if [ $yy -lt 50 ] ; then
      grep $tag sst.index.20$yy | grep TMP | wgrib -i sst.20$yy -o sst.tmp -nh
    else
      grep $tag sst.index.19$yy | grep TMP | wgrib -i sst.19$yy -o sst.tmp -nh
    fi
     
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
      $EXDIR/northtochar nout.$tag a; mv a nout.$tag
      $EXDIR/southtochar sout.$tag a; mv a sout.$tag
      $EXDIR/filt.old sst nout.$tag sout.$tag $OUTDIR/latlon.$tag \
                      $OUTDIR/nps.$tag $OUTDIR/sps.$tag $CDIR/newland \
                      $SSTFILTK $CDIR/nland.filt $CDIR/sland.filt
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
echo second pass
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
