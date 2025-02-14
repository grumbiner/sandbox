#Create initial condition files for the ice model
#Need to import CHOME, pole, OUTDIR, SSDIR EXDIR
set -xe

#cd /tmp/wd21rg
BASE=$HOME41
IDIR=/marine/ice/ice.analy

if [ -z "$CHOME" ] ; then
  CHOME=$BASE/icemodel/running
  export CHOME
fi
if [ -z "$pole" ] ; then
#  pole=north
#  export pole  
  echo pole is $pole and would be defaulted to north, causing problems for
  echo later life.    
  exit -1
fi
if [ -z "$OUTDIR" ] ; then
  OUTDIR=/eldisk1/wd21rg/iceout2
  export OUTDIR
fi 
if [ -z "$SSDIR" ] ; then
  SSDIR=$BASE/ssmi
  export SSDIR
fi
if [ -z "$EXDIR" ] ; then
  EXDIR=$BASE/execs
  export EXDIR
fi

echo $EXDIR $SSDIR $OUTDIR $pole $CHOME

#reduce $SSDIR/ssmi to icegrid
#get ssts
set -x
if [ $1 = ".TRUE." ] ; then
  echo 2 > redin ; echo 5 >> redin
  #$EXDIR/reducer umasknorth.$tag umasksouth.$tag red.north red.south < redin 
  $EXDIR/reducer $IDIR/b3north.$tag $IDIR/b3south.$tag red.north red.south < redin 
fi

echo $1 > sanein
echo $2 >> sanein

assign -a red.$pole -s unblocked fort.10
assign -a sstout fort.11

assign -a $CHOME/$pole/tsshal fort.12
assign -a $CHOME/$pole/tsdeep fort.13
assign -a $OUTDIR/restart.$pole fort.14
assign -a $OUTDIR/restart.$pole.clean fort.15

#cat sanein | $EXDIR/restart.$pole > restout
cat sanein | $EXDIR/restart.$pole 
mv $OUTDIR/restart.$pole.clean $OUTDIR/restart.$pole
