#!/bin/sh
#Operational sea ice forecast model control script
#QSUB -o /dm/wd21rg/fullfore.out
#limits should be derived from resolution. note.
#QSUB -lT  195
#QSUB -lM    4.0Mw
#QSUB -me

#Note that for operations, ic.jcl and met.jcl must be inlined

if [ $# -lt 1 ] ; then
  echo Fatal -- no month argument!
  exit -1
fi

if [ -f /etc/linux ] ; then
  RUNDIR=/data/forecasts/new/icetest
  #FBASE=/data/forecasts/sflux/${1}
  FBASE=/data/meteo/${1}
  EXDIR=/home/wd21rg/icemodel/execs
  REFDIR=/home/wd21rg/icemodel/running
  OUTBASE=$RUNDIR
  HINDBASE=$RUNDIR/hind
  PATH=$PATH:/usr/local/bin/netpbm
  #SSDIR=/data/sflux
  SSDIR=/data/forecasts/new/ssmi
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

set -x

tag=${1}01
tagm=`expr $tag - 1`
tagm=`dtgfix3 $tagm`
mm=`echo $tag | cut -c3-4`
if [ $mm -eq 1 -o $mm -eq 3 -o $mm -eq 5 -o $mm -eq 7 -o $mm -eq 8 \
                -o $mm -eq 10 -o $mm -eq 12 ] ; then
  mdy=31
else 
  if [ $mm -eq 09 -o $mm -eq 11 -o $mm -eq 04 ] ; then
    mdy=30
  else
    mdy=28
  fi
fi

echo mday = $mdy
export tag tagm mm mdy RUNDIR REFDIR EXDIR OUTBASE FBASE HINDBASE SSDIR

# Set up a restart file with initial ice thicknesses + concentrations
#Change arg list to sst, conc, restart, concweight, sstweight, hthick
#
#pole=north;  export pole
#OUTDIR=$OUTBASE/$pole
#export OUTDIR
#$EXDIR/ic.jcl .TRUE. .TRUE. .FALSE.  0.8 0.4 3.0
#
#pole=south;  export pole
#OUTDIR=$OUTBASE/$pole
#export OUTDIR
#$EXDIR/ic.jcl .TRUE. .TRUE. .FALSE.  0.8 0.4 0.6
#
#exit

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
while [ $tag -le ${1}${mdy} ] 
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
    if [ ! -f stossel.${pole}.6 ] ; then
      cp ${EXDIR}/stossel.${pole}.6 .
    fi
    
    # the f option means force the link.  i.e., do it even if the file
    #  already exists
    ln -sf  ${REFDIR}/${pole}/MASK .
    ln -sf  ${REFDIR}/${pole}/tsdeep .
    ln -sf  ${REFDIR}/${pole}/tsshal .
    ln -sf  ${REFDIR}/${pole}/bathy.$pole bathy

  #If the hindcast stage has run, use that.  Otherwise, run from the
  #  last restart file present here
    if [ ! -d $HINDBASE/$pole ] ; then
      mkdir -p $HINDBASE/$pole 
    fi
    if [ -s $HINDBASE/$pole/restart.$pole ] ; then
      cp $HINDBASE/$pole/restart.$pole $OUTBASE/$pole
     else
      echo Failed to find a hindcast restart file! 
      $EXDIR/ic.jcl .TRUE. .TRUE. .FALSE.  0.8 0.4 0.6
      cp restart.$pole $OUTBASE/$pole
    fi  
  
  #Collect the forecast files
  #Attach files to unit numbers
  #for time in 12 24 36 48 60 72 84 96 108 120 132 144 156 168 \
  #           180 192 204 216 228 240 252 264 276 288 300 312 324 \
  #           336 348 360 372 384
  for time in 00 06 12 18
  #for time in 12 24
  do
  
    # if there isn't already a met file, create it.  If there is, link to it.
    if [ ! -f ${FBASE}/met.${pole}.${tag}${time} ] ; then
      #  time ${EXDIR}/met.jcl ${FBASE}${time}
      #  time ${EXDIR}/met.jcl ${FBASE}/flxf06.${tag}${time}
      time ${EXDIR}/met.jcl ${FBASE}/${tag}${time}.fx6 
      tagm2=`expr $tagm - 1`
      tagm2=`dtgfix3 $tagm2`
      if [ -f ${FBASE}/met.${pole}.${tagm}$time ] ; then
        ln -s ${FBASE}/met.${pole}.${tagm}$time metout
      else 
        if [ -f ${FBASE}/met.${pole}.${tagm2}$time ] ; then
          ln -s ${FBASE}/met.${pole}.${tagm2}$time metout
        else 
          time ${EXDIR}/met.jcl ${FBASE}/${tag}${time}.fx6
          cp metout.${pole} ${FBASE}/met.${pole}.${tag}$time
          mv metout.${pole} metout
        fi
      fi
    else
      if [ -f metout ] ; then
        rm metout
      fi
      ln -s ${FBASE}/met.${pole}.${tag}$time metout
    fi
  
    chmod 644 metout
  
  #  set -x
    if [ -s $OUTBASE/$pole/restart.$pole ] ; then
      # Order is: concentration, SST, Restart file present
      if [ $time -eq 00 ] ; then
         $EXDIR/ic.jcl .FALSE. .TRUE.  .TRUE.  0.8 0.5 0.1
      else
        $EXDIR/ic.jcl .FALSE. .FALSE. .TRUE.  0.8 0.5 0.1
      fi
      cp $OUTBASE/$pole/restart.$pole RESTARTo
      echo ${REFDIR}/${pole}/runparm.${pole} > runin
     else
      touch RESTARTo
      echo ${REFDIR}/${pole}/runzero.${pole} > runin
    fi
  
  #Note the following addition to distinguish between hindcast (6 hrs) and
  # forecast (12 hour dt) versions.  Inelegant.
    time ./stossel.${pole}.6 < runin
  
  #Fields for general use, to be overwritten
    cp conc     $OUTBASE/$pole/conc.$pole.$tag$time
    cp RESTARTn $OUTBASE/$pole/restart.$pole
    cp RESTARTn $OUTBASE/$pole/$tag/restart.$pole
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
    #tar cvf $FDIR/f$time.tar RESTARTn thick conc vels atm.flux oce.flux \
    #                         hml sml tml FORT.16 FORT.18 
    rm  RESTARTn thick conc vels atm.flux oce.flux \
                             hml sml tml FORT.16 FORT.18 
  
    done
      #above finishes run through time for a specific region
  
      cd $OUTBASE/$pole
      if [ -s $OUTBASE/$pole/conc.$pole.${tag}168 ] ; then
        if [ -f $OUTBASE/$pole/conc.$pole.${tagm}168 ] ; then
          rm $OUTBASE/$pole/conc.$pole.${tagm}?? 
          rm $OUTBASE/$pole/conc.$pole.${tagm}???
        fi
        #${EXDIR}/www.jcl
      fi
  
      cd $FDIR
      #tar cvf t$tag.$pole.tar f*.tar
      #rm f*.tar
  
  
    done
  
    #finished all regions
    tagm=$tag
    tag=`expr $tag + 1`
    tag=`dtgfix3 $tag`
  
done 
cd $REFDIR
cd ../figs
time ./monwww.jcl $1 
