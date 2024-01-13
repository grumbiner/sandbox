#Create initial condition files for the ice model
#Need to import CHOME, pole, OUTDIR, SSDIR EXDIR
set -xe

a1=$1
a2=$2
cd $TDIR

if [ -z "$CHOME" ] ; then
  CHOME=$HOME3/icemodel/running
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
  OUTDIR=/dm/wd21rg/iceout2
  export OUTDIR
fi 
if [ -z "$SSDIR" ] ; then
  SSDIR=$HOME3/ssmi
  export SSDIR
fi
if [ -z "$EXDIR" ] ; then
  EXDIR=$HOME3/execs
  export EXDIR
fi

echo $EXDIR $SSDIR $OUTDIR $pole $CHOME

#reduce $SSDIR/ssmi to icegrid
#get ssts
set -x
if [ $a1 = ".TRUE." ] ; then
  echo 2 > redin ; echo 5 >> redin
  if [ ! -s b3north.$tag -o ! -s b3south.$tag ] ; then
    if [ -s /ombptmp/ice/analy/fifteenth/b3north.$tag ] ; then
      cp /ombptmp/ice/analy/fifteenth/b3south.$tag .
      cp /ombptmp/ice/analy/fifteenth/b3north.$tag .
      $EXDIR/reducer b3north.$tag b3south.$tag red.north red.south < redin 
     else
      a1=.FALSE.
    fi
   else 
    $EXDIR/reducer b3north.$tag b3south.$tag red.north red.south < redin 
  fi
fi
#Get the ssts if necessary
if [ $a2 = ".TRUE." ] ; then
  if [ ! -s sstout ] ; then
    fetch oned -mVS -fTR -t"dsn=nmc.prod.sstoi.analysis,disp=shr"
    assign -a oned -F null -N ibm fort.11
    assign -a sstout -s unblocked fort.12
    $EXDIR/oisst
  fi
fi
if [ ! -s sstout ] ; then
  a2=.FALSE.
fi

echo $a1 > sanein
echo $a2 >> sanein
echo 0.75 >> sanein  #weight fairly highly the observations of concentration.
echo 0.4  >> sanein  #this is a very low weight for the sst obs.

#assign -a red.$pole -s unblocked fort.10
#assign -a sstout -s unblocked fort.11
#assign -a errdel.$pole        fort.20

ln -s $CHOME/$pole/tsshal fort.12
ln -s $CHOME/$pole/tsdeep fort.13
ln -s $OUTDIR/restart.$pole fort.14
ln -s $OUTDIR/restart.$pole.clean fort.15

cat sanein | $EXDIR/restart.$pole 
mv $OUTDIR/restart.$pole.clean $OUTDIR/restart.$pole

