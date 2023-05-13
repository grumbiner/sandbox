#Create initial condition files for the ice model
#Need to import CHOME, pole, OUTDIR, SSDIR EXDIR
set -xe

a1=.FALSE.
a2=.TRUE.
pole=south

#Get the ssts if necessary
if [ $a2 = ".TRUE." ] ; then
  if [ ! -s sstout ] ; then
    fetch oned -mVS -fTR -t"dsn=nmc.prod.sstoi.analysis,disp=shr"
    assign -a oned -F null -N ibm fort.11
    assign -a sstout -s unblocked fort.12
    $HOME3/execs/oisst
  fi
fi

echo $a1 > sanein
echo $a2 >> sanein
echo 0.75 >> sanein  #weight fairly highly the observations of SST.
echo 0.4  >> sanein  #this is a very low weight for the sst obs.

assign -a red.$pole -s unblocked fort.10
assign -a sstout -s unblocked fort.11
assign -a errdel.$pole        fort.20

CHOME=$HOME3/icemodel/running
OUTDIR=$DHOME/rerun
assign -a $CHOME/$pole/tsshal fort.12
assign -a $CHOME/$pole/tsdeep fort.13
assign -a $OUTDIR/restart.$pole fort.14
assign -a $OUTDIR/restart.$pole.clean fort.15

cat sanein | ./restart.$pole 
