#!/bin/sh

RUNDIR=/export/s2/tmp
FBASE=/data/forecasts/ice.model/forcing/
EXDIR=/data/forecasts/ice.model/crays/execs
REFDIR=/data/forecasts/ice.model/crays/icemodel/running
OUTDIR=/data/new/output
HINDDIR=$OUTDIR
CHOME=$REFDIR
METDIR=/data/ssmi/meteo
export tag RUNDIR REFDIR EXDIR OUTDIR FBASE HINDDIR METDIR CHOME

#Nothing below this point should need changing -------------------------

if [ ! -d $RUNDIR ] ; then
  mkdir $RUNDIR
fi
cd $RUNDIR

#set -xe
#for pole in north south glk 


cd $RUNDIR
rm *

tag=950930
tagm=950929
rm $OUTDIR/restart.south
while [ $tag -lt 960531 ]
do
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
  tagm=`expr $tagm + 1`
  tagm=`dtgfix3 $tagm`
  yymm=`echo $tag | cut -c1-4`

  for pole in south 
  do
    export pole
    #Make sure the stossel model is here
    if [ ! -f stossel.${pole}.6 ] ; then
      cp ${EXDIR}/stossel.${pole}.6 .
      cp ${REFDIR}/${pole}/MASK .
      cp ${REFDIR}/${pole}/tsdeep .
      cp ${REFDIR}/${pole}/tsshal .
      cp ${REFDIR}/${pole}/bathy.$pole bathy
    fi
    #If the hindcast stage has run, use that.  Otherwise, run from the
    #  last restart file present here
    #if [ -s $HINDDIR/restart.$pole ] ; then
    #  cp $HINDDIR/restart.$pole $OUTDIR
    # else
    #  echo Failed to find a hindcast restart file! 
    #fi  

    #Collect the forecast files
    #Attach files to unit numbers
    for time in 00 06 12 18
    do
    
#      ${EXDIR}/met.jcl ${FBASE}/${yymm}/flxf06.${tag}${time}
    
      cp ${METDIR}/met${pole}.${tag}${time} metout

      set -x
      if [ -s $OUTDIR/restart.$pole ] ; then
        ${EXDIR}/ic.jcl .FALSE. .FALSE.

        #if [ $time -eq 12 ] ; then
        #  #$HOME3/icemodel/ic/ic.jcl .TRUE. .TRUE.
        #else
        #  $HOME3/icemodel/ic/ic.jcl .FALSE. .FALSE.
        #fi
        cp $OUTDIR/restart.$pole RESTARTo
        echo ${REFDIR}/${pole}/runparm.${pole} > runin
       else
        touch RESTARTo
        echo ${REFDIR}/${pole}/runzero.${pole} > runin
      fi
    
      time stossel.${pole}.6 < runin
    
    #Fields for general use, to be overwritten
      cp conc     $OUTDIR/conc.$pole.$tag$time
      #if [ $time -eq 24 ] ; then
        cp RESTARTn $OUTDIR/restart.$pole
      #fi
    
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
  #finished all regions

done
