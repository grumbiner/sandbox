#!/bin/ksh
#Script to conduct a single forecast for a date given in the argument

set -x
if [ $# -lt 1 ] ; then
  echo Fatal -- no date given to forecast for!
  exit -1
fi

HARDSTART=no
CC=19
TAROP=z
#     TAROP is used for high resolution model versions, to compress on fly
#     The if test is used for very high resolution model version, to get
#        just the last fcst, valid 00Z next day.
ANUDGE=0.5     #Ice concentration nudging for 00Z
OFFNUDGE=0.00  #Ice concentration nudging for off times
export CC TAROP HARDSTART ANUDGE OFFNUDGE

base=$1

if [ -f /etc/linux ] ; then
  FBASE=/home/rmg3/icemodel/icemodel/forcing/fcst
  SSDIR=/home/rmg3/icemodel/icemodel/ssmi
  EXDIR=/home/rmg3/icemodel/icemodel/execs
  REFDIR=/home/rmg3/icemodel/icemodel/running
  RUNDIR=/data/forecasts/new/icetest
  OUTBASE=$RUNDIR/fcst
  HINDBASE=$RUNDIR/
  PATH=$PATH:/usr/local/bin/netpbm
  LANDDIR=/home/rmg3/ssmi/land
else
#Assumed to be ibm for now
  RUNDIR=/tmp/wd21rg
  FBASE=/u/wx21rg/icemodel/icemodel/forcing/${base}
  EXDIR=/u/wx21rg/icemodel/icemodel/execs
  REFDIR=/u/wx21rg/icemodel/icemodel/running
  SSDIR=/u/wx21rg/icemodel/icemodel/ssmi
  LANDDIR=/u/wx21rg/icemodel/land
  OUTBASE=/u/wx21rg/iceout2
  HINDBASE=/u/wx21rg/iceout2/hind
fi

#Nothing below this point should need changing -------------------------
#set -x

tag=${base}
tagm=`expr $tag - 1`
tagm=`dtgfix3 $tagm`

export tag tagm RUNDIR REFDIR EXDIR OUTBASE FBASE HINDBASE SSDIR LANDDIR

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

# Set up a restart file with initial ice thicknesses + concentrations
#Change arg list to sst, conc, restart, concweight, sstweight, hthick
#  restart = true means run from restart file
#
#set -x
if [ $HARDSTART = 'yes' ] ; then
  for pole in north south 
  do
    OUTDIR=$OUTBASE/$pole
    export pole OUTDIR
    if [ ! -d $OUTDIR ] ; then
      mkdir -p $OUTDIR
    fi
    echo About to try to run the ic checker, $pole
    $EXDIR/ic.sh .TRUE. .TRUE. .FALSE.  $ANUDGE 0.5 0.5
    cp $OUTBASE/$pole/restart.$pole $HINDBASE/$pole
  done
fi
#exit

#echo `time  $EXDIR/getmet.jcl` getmet.jcl
  export tag tagm
  for pole in north south 
  do
    cd $RUNDIR
    OUTDIR=$OUTBASE/$pole
    export pole OUTDIR
    rm *
  #Make sure the stossel model is here
    if [ ! -s stossel.${pole}.12 ] ; then
      cp ${EXDIR}/stossel.${pole}.12 .
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
    if [ -s $HINDBASE/$pole/$tag/restart.$pole ] ; then
      cp $HINDBASE/$pole/$tag/restart.$pole $OUTBASE/$pole
      echo working from restart file
     else
      echo Failed to find a hindcast restart file! 
      $EXDIR/ic.sh .TRUE. .TRUE. .FALSE.  0.875 0.5 0.6
      cp restart.$pole $OUTBASE/$pole
    fi  
  
    #Collect the forecast files
    for time in 00 12 24 36 48 60 72 84 96 108 120
    do
  
      # if there is not already a met file, create it.  If there is, link to it.
      if [ ! -s ${FBASE}/met.${pole}.${tag}${time} ] ; then
        #${EXDIR}/met.jcl ${FBASE}/${CC}${tag}${time}.fx6 
        #hcast ${EXDIR}/met.jcl ${FBASE}/flxf06.${CC}${tag}${time}
        ${EXDIR}/met.jcl ${FBASE}/flxf${time}.${CC}${tag}00
        cp metout.${pole} ${FBASE}/met.${pole}.${tag}$time 
        tagm2=`expr $tagm - 1`
        tagm2=`dtgfix3 $tagm2`
        if [ -s met.${pole}.${tagm}$time ] ; then
          ln -s met.${pole}.${tagm}$time metout
          cp met.${pole}.${tagm}$time $FBASE
        else 
          if [ -s met.${pole}.${tagm2}$time ] ; then
            ln -s met.${pole}.${tagm2}$time metout
            cp met.${pole}.${tagm2}$time $FBASE
          else 
            #hcast ${EXDIR}/met.jcl ${FBASE}/${CC}${tag}${time}.fx6
            ${EXDIR}/met.jcl ${FBASE}/flxf${time}.${CC}${tag}00
            cp metout.${pole} met.${pole}.${tag}$time
            cp met.${pole}.${tag}$time $FBASE
            mv metout.${pole} metout
          fi
        fi
      else
        if [ -s metout ] ; then
          rm metout
        fi
        ln -s ${FBASE}/met.${pole}.${tag}$time metout
      fi
  
      chmod 644 metout
  
      if [ -s $OUTBASE/$pole/restart.$pole ] ; then
        # Order is: concentration, SST, Restart file present
        if [ $time -eq 00 ] ; then
          echo calling time zero ic check
          $EXDIR/ic.sh .TRUE. .TRUE. .TRUE.  $ANUDGE 0.25 0.1
        else
          echo calling off time ic check
          $EXDIR/ic.sh .TRUE. .TRUE. .TRUE.  $OFFNUDGE 0.25 0.1
        fi
        cp $OUTBASE/$pole/restart.$pole RESTARTo
        echo ${REFDIR}/${pole}/runparm.${pole} > runin
       else
        touch RESTARTo
        echo calling in-line ic check and restart generation
        $EXDIR/ic.sh .TRUE. .TRUE. .FALSE.  0.875 0.5 0.1
        echo ${REFDIR}/${pole}/runzero.${pole} > runin
      fi
  
    #Note the following addition to distinguish between hindcast (6 hrs) and
    # forecast (12 hour dt) versions.  Inelegant.
      time ./stossel.${pole}.12 < runin
    
    #Fields for general use, to be overwritten
      cp conc     $OUTBASE/$pole/conc.$pole.$tag$time
      cp RESTARTn $OUTBASE/$pole/restart.$pole
      #mv RESTARTn $HINDBASE/$pole/restart.$pole  Do not modify HINDBASE from
      #    forecast
  
    #Fields for archival
      if [ ! -d $OUTBASE/$pole ] ; then
        mkdir $OUTBASE/$pole
      fi
      FDIR=$OUTBASE/$pole/${tag}
      if [ ! -d $FDIR ] ; then
        mkdir $FDIR
      fi
      #tar c${TAROP}vf $FDIR/f$time.tar${TAROP} $OUTBASE/$pole/restart.$pole \
      tar c${TAROP}vf $FDIR/f$time.tar${TAROP} \
                               thick conc vels \
                               atm.flux oce.flux \
                               hml sml tml FORT.16 FORT.18 
      rm  thick conc vels atm.flux oce.flux \
                           hml sml tml FORT.16 FORT.18

      done
    #above finishes run through time for a specific region
  
    done
  
    #finished all regions
