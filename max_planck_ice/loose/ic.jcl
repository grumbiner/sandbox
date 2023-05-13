#Create initial condition files for the ice model
#Need to import REFDIR, pole, OUTDIR, SSDIR EXDIR
#Note that this script is over tailored to the north/south hemisphere runs
#Robert Grumbine
#Last Modified 20 February 1997

#set -x

a1=$1 #Logical for whether to use the ice concentrations ic
a2=$2 #Logical for whether to use the sst ic
a3=$3 #Logical for whether to create a new restart file from scratch

if [ -z "$tag" ] ; then
  echo "No date set for initial conditions!"
  exit -1
fi

if [ -z "$RUNDIR" ] ; then
  echo "No run directory set "
  exit -1
fi

cd $RUNDIR

echo $EXDIR $SSDIR $OUTDIR $pole $REFDIR $RUNDIR

#reduce $SSDIR/ssmi to icegrid
#get ssts
#Need version of reduce that is pole independant
#Need to run off gribbed ice concentrations 

#set -x
if [ $a1 = ".TRUE." ] ; then
  base=`echo $tag | cut -c1-4` 
  if [ ! -s northpsg.$tag -o ! -s southpsg.$tag ] ; then
    if [ -s $SSDIR/npsg.$base ] ; then
      #Try a month's backup file
      cp $SSDIR/spsg.$base .
      cp $SSDIR/npsg.$base .
      wgrib npsg.$base | grep $tag | wgrib -i npsg.$base -o b3north.$tag
      wgrib spsg.$base | grep $tag | wgrib -i spsg.$base -o b3south.$tag
      $EXDIR/reduce b3north.$tag b3south.$tag red.north red.south 
      #rm southpsg.$tag northpsg.$tag b3???th.$tag
     else
      a1=.FALSE.
    fi
   else 
      wgrib northpsg.$tag | wgrib -i northpsg.$tag -o b3north.$tag
      wgrib southpsg.$tag | wgrib -i southpsg.$tag -o b3south.$tag
      $EXDIR/reduce b3north.$tag b3south.$tag red.north red.south 
      rm southpsg.$tag northpsg.$tag b3???th.$tag
  fi
fi

#Get the ssts if necessary
# Something will need to be done re. getting daily 1 degree fields or
# replacing them with 0.5 degree fields.
if [ $a2 = ".TRUE." ] ; then
    base=`echo $tag | cut -c1-2` 
    wgrib $SSDIR/sst/sst.19$base | grep $tag |     \
             wgrib -i $SSDIR/sst/sst.19$base -o sstout
fi

if [ ! -s sstout ] ; then
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


if [ -f fort.10 -o -f fort.11 -o -f fort.12 -o -f fort.13 -o -f fort.14 -o -f fort.15 ] ; then
  rm fort.1?
fi
if [ ! -d $OUTDIR ] ; then
  mkdir -p $OUTDIR
fi
ln -s red.$pole fort.10
ln -s sstout fort.11
ln -s $REFDIR/$pole/tsshal fort.12
ln -s $REFDIR/$pole/tsdeep fort.13
ln -s $REFDIR/$pole/bathy.$pole fort.17
ln -s $OUTDIR/restart.$pole fort.14
echo "-0.16 15.0 -1.8 33.3 70.0 20.0" > fort.16

cat sanein | $EXDIR/restart.$pole 
cp fort.15 $OUTDIR/restart.$pole
#mv fort.15 $HINDBASE/$pole/restart.$pole

