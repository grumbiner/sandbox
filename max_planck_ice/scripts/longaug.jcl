#!/bin/sh
#Operational sea ice forecast model control script
#QSUB -o /dm/wd21rg/fullfore.out
#limits should be derived from resolution. note.
#QSUB -lT  195
#QSUB -lM    4.0Mw
#QSUB -me


if [ -f /etc/linux ] ; then
  RUNDIR=/data/new/icetest
  #FBASE=/cdrom/9701/
  FBASE=/data/sflux/
  EXDIR=~rmg3/execs
  REFDIR=~rmg3/icemodel/running
  OUTBASE=$RUNDIR
  HINDBASE=$RUNDIR/hind
  PATH=$PATH:/usr/local/bin/netpbm
  #SSDIR=/data/sflux
  SSDIR=/home/ftp/pub/ice
else
#Assumed to be cray3 for now
  RUNDIR=/tmp/wd21rg
  FBASE=/com/mrf/prod/mrf.${tag}/drfmr.T00Z.SFLUXGrb
  EXDIR=/wd2/wd21/wd21rg/execs
  REFDIR=/wd2/wd21/wd21rg/icemodel/running
  OUTBASE=/ombptmp/ice/iceout2
  HINDBASE=/eldisk1/wd21rg/iceout2/hind
  SSDIR=/ombptmp/ice/analy/fifteenth
fi

tag=970801
tagm=970731
export tag RUNDIR REFDIR EXDIR OUTBASE FBASE HINDBASE SSDIR

#Nothing below this point should need changing -------------------------
for d in $RUNDIR $OUTBASE $HINDBASE
do
  if [ ! -d $d ] ; then
    mkdir -p $d
  fi
done
 
if [ ! -d $REFDIR ] ; then
  echo Cannot find the reference directory.  Cannot run.
  exit -1
fi 
if [ ! -d $EXDIR ] ; then
  echo Cannot find the executables directory.  Cannot run.
  exit -1
fi
if [ ! -d $SSDIR ] ; then
  echo Cannot find the SSMI directory.  Cannot run.
  exit -1
fi 

cd $RUNDIR

#echo `time  $EXDIR/getmet.jcl` getmet.jcl
while [ $tag -le 970808 ] 
do
  export tag tagm
#set -xe
#for pole in south north glk 
for pole in south north 
do
  cd $RUNDIR
  OUTDIR=$OUTBASE/$pole
  export pole OUTDIR
  rm *
#Make sure the stossel model is here
  cp ${EXDIR}/stossel.${pole}.6 .
  cp ${REFDIR}/${pole}/MASK .
  cp ${REFDIR}/${pole}/tsdeep .
  cp ${REFDIR}/${pole}/tsshal .
  cp ${REFDIR}/${pole}/bathy.$pole bathy
#If the hindcast stage has run, use that.  Otherwise, run from the
#  last restart file present here
  if [ -s $HINDBASE/$pole/restart.$pole ] ; then
    cp $HINDBASE/$pole/restart.$pole $OUTBASE/$pole
   else
    echo Failed to find a hindcast restart file! 
  fi  

#Collect the forecast files
#Attach files to unit numbers
#for time in 12 24 36 48 60 72 84 96 108 120 132 144 156 168 \
#           180 192 204 216 228 240 252 264 276 288 300 312 324 \
#           336 348 360 372 384
for time in 00 06 12 18
#for time in 12 24
do

#  time ${EXDIR}/met.jcl ${FBASE}${time}
#  time ${EXDIR}/met.jcl ${FBASE}/${tag}${time}.fx6
  time ${EXDIR}/met.jcl ${FBASE}/flxf06.${tag}${time}

  cp metout.${pole} metout
  chmod 644 metout

#  set -x
  if [ -s $OUTBASE/$pole/restart.$pole ] ; then
    if [ $time -eq 00 ] ; then
      $EXDIR/ic.jcl .TRUE. .TRUE.
#      $EXDIR/ic.jcl .FALSE. .FALSE.
    else
      $EXDIR/ic.jcl .FALSE. .FALSE.
    fi
    cp $OUTBASE/$pole/restart.$pole RESTARTo
    echo ${REFDIR}/${pole}/runparm.${pole} > runin
   else
    touch RESTARTo
    echo ${REFDIR}/${pole}/runzero.${pole} > runin
  fi

#Note the following addition to distinguish between hindcast (6 hrs) and
# forecast (12 hour dt) versions.  Inelegant.
  time stossel.${pole}.6 < runin

#Fields for general use, to be overwritten
  cp conc     $OUTBASE/$pole/conc.$pole.$tag$time
  cp RESTARTn $OUTBASE/$pole/restart.$pole
#  if [ $time -eq 24 ] ; then
    cp RESTARTn $HINDBASE/$pole/restart.$pole
#  fi

#Fields for archival
  if [ ! -d $OUTBASE/$pole ] ; then
    mkdir $OUTBASE/$pole
  fi
  FDIR=$OUTBASE/$pole/${tag}
  if [ ! -d $FDIR ] ; then
    mkdir $FDIR
  fi
  tar cvf $FDIR/f$time.tar RESTARTn thick conc vels atm.flux oce.flux \
                           hml sml tml FORT.16 FORT.18 
  rm  RESTARTn thick conc vels atm.flux oce.flux \
                           hml sml tml FORT.16 FORT.18 

done
#above finishes run through time for a specific region

cd $OUTBASE/$pole
if [ -s $OUTBASE/$pole/conc.$pole.${tag}168 ] ; then
  if [ -f $OUTBASE/$pole/conc.$pole.${tagm}168 ] ; then
    rm $OUTBASE/$pole/conc.$pole.${tagm}?? $OUTBASE/$pole/conc.$pole.${tagm}???
  fi

  ${EXDIR}/www.jcl

fi

cd $FDIR
tar cvf t$tag.$pole.tar f*.tar
rm f*.tar


done
#finished all regions
  tagm=$tag
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`

done 
