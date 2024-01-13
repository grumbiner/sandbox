#!/bin/sh
#Operational sea ice forecast model control script
#QSUB -o /dm/wd21rg/fullfore.out
#limits should be derived from resolution. note.
#QSUB -lT  195
#QSUB -lM    4.0Mw
#QSUB -me

tag=`date +'%y%m%d'`
tagm=`expr $tag - 1 `
tagm=`dtgfix3 $tagm `

#RUNDIR=/tmp/wd21rg
#FBASE=/com/mrf/prod/mrf.${tag}/drfmr.T00Z.SFLUXGrb
#EXDIR=/wd2/wd21/wd21rg/execs
#REFDIR=/wd2/wd21/wd21rg/icemodel/running
#OUTDIR=/ombptmp/ice/iceout2
#HINDDIR=/eldisk1/wd21rg/iceout2

RUNDIR=/export/s2/tmp
FBASE=/export/s2/mrf/drfmr.T00Z.SFLUXGrbF
EXDIR=~rmg3/execs
REFDIR=~rmg3/icemodel/running
OUTDIR=$RUNDIR/glk
HINDDIR=$RUNDIR/glk
PATH=$PATH:/usr/local/bin/netpbm
export tag RUNDIR REFDIR EXDIR OUTDIR FBASE HINDDIR

#Nothing below this point should need changing -------------------------

if [ ! -d $RUNDIR ] ; then
  mkdir $RUNDIR
fi
cd $RUNDIR

time  $EXDIR/getmet.jcl 

#set -xe
#for pole in north south glk 
for pole in glk 
do
  cd $RUNDIR
  export pole
  rm *
#Make sure the stossel model is here
  cp ${EXDIR}/stossel.${pole}.12 .
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
do

#  ${EXDIR}/met.jcl ${FBASE}${time}.${tag}00
  time ${EXDIR}/met.jcl ${FBASE}${time}

  cp metout.${pole} metout
  chmod 644 metout

  set -x
  if [ -s $OUTDIR/restart.$pole ] ; then
    if [ $time -eq 12 ] ; then
      $EXDIR/ic.jcl .TRUE. .TRUE.
    else
      $EXDIR/ic.jcl .FALSE. .FALSE.
    fi
    cp $OUTDIR/restart.$pole RESTARTo
    echo ${REFDIR}/${pole}/runparm.${pole} > runin
   else
    touch RESTARTo
    echo ${REFDIR}/${pole}/runzero.${pole} > runin
  fi

#Note the following addition to distinguish between hindcast (6 hrs) and
# forecast (12 hour dt) versions.  Inelegant.
  time stossel.${pole}.12 < runin

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

cd $OUTDIR
if [ -s $OUTDIR/conc.$pole.${tag}168 ] ; then
  if [ -f $OUTDIR/conc.$pole.${tagm}168 ] ; then
    rm $OUTDIR/conc.$pole.${tagm}?? $OUTDIR/conc.$pole.${tagm}???
  fi

  for time in 12 24 36 48 60 72 84 96 108 120 132 144 156 168
  do
    $EXDIR/glktxpm conc.glk.$tag$time $REFDIR/$pole/bathy.glk gl$time.xpm
    /usr/local/bin/netpbm/xpmtogif gl$time.xpm
    mv gl$time.gif /data/WWW/develop/glk
    rm gl$time.xpm
  done
  if [ -s colcell1.xpm ] ; then
    rm *.xpm *.html
  fi
fi

cd $FDIR
tar cvf t$tag.$pole.tar f*.tar
rm f*.tar


done
#finished all regions
