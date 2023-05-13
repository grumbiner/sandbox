#!/bin/sh
#Operational sea ice forecast model control script
#QSUB -o /dm/wd21rg/glk.out
#limits should be derived from resolution. note.
#QSUB -lT  300
#QSUB -lM    8.0Mw
#QSUB -me

ja

tag=`date +'%y%m%d'`
tagm=`expr $tag - 1 `
tagm=`dtgfix3 $tagm `

RUNDIR=/tmp/wd21rg
FBASE=/com/mrf/prod/mrf.${tag}/drfmr.T00Z.SFLUXGrbF
EXDIR=/wd2/wd21/wd21rg/execs
REFDIR=/wd2/wd21/wd21rg/icemodel/running
OUTDIR=/ombptmp/ice/iceout2
HINDDIR=/eldisk1/wd21rg/iceout2

#RUNDIR=/export/s2/tmp
#FBASE=/home/ftp/pub/data0/flxf
#FBASE=/home/ftp/pub/data0/drfmr.T00Z.SFLUXGrbF
#EXDIR=/data/forecasts/ice.model/cray3/execs
#REFDIR=/data/forecasts/ice.model/cray3/icemodel/running
#OUTDIR=$RUNDIR
#HINDDIR=$RUNDIR
export tag RUNDIR REFDIR EXDIR OUTDIR FBASE HINDDIR

#Nothing below this point should need changing -------------------------

if [ ! -d $RUNDIR ] ; then
  mkdir $RUNDIR
fi
cd $RUNDIR

#set -xe
#for pole in north south glk 
for pole in glk 
do
  cd $RUNDIR
  export pole
#Make sure the stossel model is here
  cp ${EXDIR}/stossel.${pole} .
  cp ${REFDIR}/${pole}/MASK .
  cp ${REFDIR}/${pole}/tsdeep .
  cp ${REFDIR}/${pole}/tsshal .
  cp ${REFDIR}/${pole}/bathy.$pole bathy
#If the hindcast stage has run, use that.  Otherwise, run from the
#  last restart file present here
  if [ -s $HINDDIR/restart.$pole ] ; then
    cp $HINDDIR/restart.$pole $OUTDIR
   else
    echo Failed to find a hindcast restart file! 
  fi  

#Collect the forecast files
#Attach files to unit numbers
for time in 12 24 36 48 60 72 84 96 108 120 132 144 156 168 
#for time in 12 
do

#  ${EXDIR}/met.jcl ${FBASE}${time}.${tag}00
  ${EXDIR}/met.jcl ${FBASE}${time}

  cp metout.${pole} metout
  chmod 644 metout

  set -x
  if [ -s $OUTDIR/restart.$pole ] ; then
    if [ $time -eq 12 ] ; then
      $HOME3/icemodel/ic/ic.jcl .FALSE. .FALSE.
    else
      $HOME3/icemodel/ic/ic.glk.jcl .FALSE. .FALSE.
    fi
    cp $OUTDIR/restart.$pole RESTARTo
    echo ${REFDIR}/${pole}/runparm.${pole} > runin
   else
    touch RESTARTo
    echo ${REFDIR}/${pole}/runzero.${pole} > runin
  fi

  assign -a bathy -s unblocked fort.27
  assign -a tsdeep -s unblocked fort.14
  assign -a tsshal -s unblocked fort.13
  time stossel.${pole} < runin

#Fields for general use, to be overwritten
  cp conc     $OUTDIR/conc.$pole.$tag$time
  if [ $time -eq 24 ] ; then
    cp RESTARTn $OUTDIR/restart.$pole
  fi
#  cp FORT.18  $OUTDIR/fort18.$pole
#  cp thick    $OUTDIR/thick.$pole
#  cp vels     $OUTDIR/vels.$pole

#Fields for archival
  if [ ! -d $OUTDIR/$pole ] ; then
    mkdir $OUTDIR/$pole
  fi
#  FDIR=$OUTDIR/$pole/${tag}$time
  FDIR=$OUTDIR/$pole/${tag}
  if [ ! -d $FDIR ] ; then
    mkdir $FDIR
  fi
  tar cvf $FDIR/f$time.tar RESTARTn thick conc vels atm.flux oce.flux \
                           hml sml tml FORT.16 FORT.18 
  rm  RESTARTn thick conc vels atm.flux oce.flux \
                           hml sml tml FORT.16 FORT.18 

done
#above finishes run through time for a specific region

cd $FDIR
tar cvf t$tag.$pole.tar f*.tar
rm f*.tar
cd $RUNDIR
if [ -s $OUTDIR/conc.$pole.${tag}168 ] ; then
  if [ -f $OUTDIR/conc.$pole.${tagm}168 ] ; then
    rm $OUTDIR/conc.$pole.${tagm}?? $OUTDIR/conc.$pole.${tagm}???
  fi
fi


done

ja -chlst
#finished all regions
