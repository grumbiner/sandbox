#!/bin/ksh
#Create initial condition files for the ice model
#Need to import REFDIR, pole, OUTDIR, SSDIR EXDIR, tag
#  SSTDIR   = SST directory - data files
#  CONCDIR  = SSMI directory - data files
#Note that this script is over tailored to the north/south hemisphere runs
#Robert Grumbine
#Last Modified 20 February 1997
#Last Modified 11 March 1999

## Should need no modifications ----------------------

#set -x

echo initial flags $a1 $a2 $a3
if [ $# -lt 4 ] ; then
  a1=".FALSE."
  a2=".FALSE."
  a3=".TRUE."
  echo insufficient arguments passed, defaulting
else
  a1=$1 #whether to use the ice concentrations ic
  a2=$2 #whether to use the sst ic
  a3=$3 #whether to create a new restart file from scratch
fi


if [ -z "$tag" ] ; then
  echo "No date set for initial conditions!"
  exit -1
fi

if [ -z "$SSTDIR" ] ; then
  echo "No SST analysis directory set"
  exit -1
fi
if [ -z "$CONCDIR" ] ; then
  echo "No SSMI analysis directory set"
  exit -1
fi

if [ -z "$RUNDIR" ] ; then
  echo "No run directory set "
  exit -1
fi

echo running directory to which were trying to cd is $RUNDIR
cd $RUNDIR

echo $EXDIR $CONCDIR $SSTDIR $OUTDIR $pole $REFDIR $RUNDIR

#reduce $CONCDIR/ssmi to icegrid
#get ssts
#Need to run off gribbed ice concentrations 

#set -x
if [ $a1 = ".TRUE." ] ; then
  if [ ${#tag} -eq 8 ] ; then
    base=`echo $tag | cut -c1-6`
    gtag=`echo $tag | cut -c3-8`
  else
    base=`echo $tag | cut -c1-4` 
    gtag=$tag
  fi
  if [ ! -s northpsg.$tag -o ! -s southpsg.$tag ] ; then
    if [ -s $CONCDIR/npsg.$base ] ; then
      #Try a month's backup file
      if [ ! -f $CONCDIR/spsg.$base ] ; then 
        echo could not find ssmi file
        a1=.FALSE.
      fi
      if [ $a1 = ".TRUE." ] ; then
        wgrib ${CONCDIR}/npsg.$base | grep $gtag | \
              wgrib -i ${CONCDIR}/npsg.$base -o b3north.$tag -nh
        wgrib ${CONCDIR}/spsg.$base | grep $gtag | \
              wgrib -i ${CONCDIR}/spsg.$base -o b3south.$tag -nh
        $EXDIR/tomodel.north b3north.$tag $CONCDIR/nland red.north
        $EXDIR/tomodel.south b3south.$tag $CONCDIR/sland red.south
      fi
     else
      echo failed to find ice concentration to work with, skipping ice assim.
      echo $CONCDIR 
      a1=.FALSE.
    fi
   else 
      wgrib northpsg.$tag | wgrib -i northpsg.$tag -o b3north.$tag
      wgrib southpsg.$tag | wgrib -i southpsg.$tag -o b3south.$tag
      $EXDIR/tomodel.north b3north.$tag $CONCDIR/nland red.north
      $EXDIR/tomodel.south b3south.$tag $CONCDIR/sland red.south
      rm southpsg.$tag northpsg.$tag b3???th.$tag
  fi
fi

echo post ice conc flags $a1 $a2 $a3

#Get the ssts if necessary
# Something will need to be done re. getting daily 1 degree fields or
# replacing them with 0.5 degree fields.
if [ $a2 = ".TRUE." ] ; then
  if [ ${#tag} -eq 8 ] ; then
    base=`echo $tag | cut -c1-4`
    stag=`echo $tag | cut -c3-8`
  else
    base=`echo $tag | cut -c1-2`
    stag=$tag
  fi
    if [ -s $SSTDIR/sst.$base ] ; then
       wgrib $SSTDIR/sst.$base | grep $stag |     \
         wgrib -i $SSTDIR/sst.$base -o sstout -nh
     else
       echo failed to find $SSTDIR/sst.$base skipping sst assimilation
       a2=.FALSE.
    fi
fi

if [ ! -s sstout ] ; then
  echo failed to construct sstout, skipping sst assimilation
  a2=.FALSE.
fi


echo $a1 > sanein
echo $a2 >> sanein
echo $a3 >> sanein
if [ $# -le 4 ] ; then
  echo 0.75 >> sanein  #weight fairly highly the observations of concentration.
  echo 0.4  >> sanein  #this is a very low weight for the sst obs.
  echo 3.0  >> sanein  #this is the thickness to set the ice to in restart
else
  echo $4 >> sanein
  echo $5 >> sanein
  echo $6 >> sanein
fi


if [ -f fort.10 -o -f fort.11 -o -f fort.12 -o -f fort.13 -o -f fort.14 -o \
     -f fort.15 ] ; then
  rm fort.1?
fi

if [ ! -d $OUTDIR ] ; then
  mkdir -p $OUTDIR
fi

ln -sf red.$pole fort.10
ln -sf sstout fort.11
ln -sf $REFDIR/$pole/tsshal fort.12
ln -sf $REFDIR/$pole/tsdeep fort.13
ln -sf $REFDIR/$pole/bathy.$pole fort.17
ln -sf $OUTDIR/restart.$pole fort.14
echo "-0.16 15.0 -1.8 33.3 70.0 20.0" > fort.16

echo flags final: $a1 $a2 $a3
time cat sanein | $EXDIR/restart.$pole 
cp fort.15 $OUTDIR/restart.$pole
cp fort.15 restart.$pole
mv fort.15 $HINDBASE/$pole/restart.$pole
