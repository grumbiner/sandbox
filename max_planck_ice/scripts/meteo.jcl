#!/bin/sh

RUNDIR=/export/s2/tmp
FBASE=/data/forecasts/ice.model/forcing/
EXDIR=/data/forecasts/ice.model/crays/execs
REFDIR=/data/forecasts/ice.model/crays/icemodel/running
OUTDIR=/data/new/output
HINDDIR=$OUTDIR
METDIR=/data/ssmi/meteo
export tag RUNDIR REFDIR EXDIR OUTDIR FBASE HINDDIR METDIR

#Nothing below this point should need changing -------------------------

if [ ! -d $RUNDIR ] ; then
  mkdir $RUNDIR
fi
cd $RUNDIR


tag=950930
tagm=950929
    cd $RUNDIR
while [ $tag -lt 960531 ] 
do
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
  tagm=`expr $tagm + 1`
  tagm=`dtgfix3 $tagm`
  yymm=`echo $tag | cut -c1-4`

  pole=glk
  export pole

    #Collect the forecast files
    #Attach files to unit numbers
    for time in 00 06 12 18
    do
    
      time ${EXDIR}/met.jcl ${FBASE}/${yymm}/flxf06.${tag}${time}
    
      mv metout.${pole} $METDIR/met${pole}.${tag}${time}

    done

done
