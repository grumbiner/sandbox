#!/bin/ksh
#Combined script: Conduct hindcast and forecast for a date given in
#  script argument.
#Robert Grumbine 19 March 1999

#Ensure that we have a date to forecast for:
if [ $# -lt 1 ] ; then
  echo Fatal -- no date given to forecast for!
  exit -1
fi
base=$1


#Establish run parameters:
#  HARDSTART= yes in order to make a cold model restart, set from outside script
#              or modify default.
#  CC       = current leading two digits of year
#  TAROP    = to compress output tar files on fly
#  ANUDGE   = Nudging to use for the initial assessment of ice concentration 
#  OFFNUDGE = Nudging to use during off time ice conc. nudging
HARDSTART=${HARDSTART:-no}
CC=${CC:-19}
TAROP=${TAROP:-z}
ANUDGE=${ANUDGE:-0.5}
OFFNUDGE=${OFFNUDGE:-0.00}
export base CC TAROP HARDSTART ANUDGE OFFNUDGE

#Now set up the directory structure for the forecast/hindcast -- start with
#  hindcast values
if [ -f /etc/linux ] ; then
  SSDIR=/home/rmg3/icemodel/icemodel/ssmi
  EXDIR=/home/rmg3/icemodel/icemodel/execs
  REFDIR=/home/rmg3/icemodel/icemodel/running
  RUNDIR=/data/forecasts/new/icetest
  HINDBASE=$RUNDIR/hcst
  PATH=$PATH:/usr/local/bin/netpbm
  LANDDIR=/home/rmg3/ssmi/land
  #FBASE=/home/rmg3/icemodel/icemodel/forcing/hcst
  #OUTBASE=$RUNDIR/hcst
  FNAUGHT=/home/rmg3/icemodel/icemodel/forcing/
  OUTNAUGHT=$RUNDIR/
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
export RUNDIR REFDIR EXDIR OUTNAUGHT FNAUGHT HINDBASE SSDIR LANDDIR

#-----------------------------------------------------------------------
#Nothing below this point should need changing -------------------------
#-----------------------------------------------------------------------
#set -x

#Make directories as needed and verify that required data directories are
#  present
for d in $RUNDIR $OUTNAUGHT $HINDBASE
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
if [ ! -d $LANDDIR ] ; then
  echo Cannot find the land directory.  Cannot run.
  exit -1
fi 
if [ ! -d $SSDIR ] ; then
  echo Cannot find the SSMI directory.  Cannot run.
  exit -1
fi 

cd $RUNDIR

#Now prepare for the forecast/hindcast cycle, establish the dates:
tag=${base}
tagm=`expr $tag - 1`
tagm=`dtgfix3 $tagm`

polelist='north south'
hcsttimes='00 06 12 18'
fcsttimes='12 24 36 48 60 72 84 96 108 120'
export tag tagm polelist hcsttimes fcsttimes

for cycletype in hcst fcst
do
  for pole in $polelist
  do
    if [ ! -d $HINDBASE/$pole ] ; then
      mkdir -p $HINDBASE/$pole 
    fi
    if [ ! -d $OUTNAUGHT/$cycletype/$pole/$tag ] ; then
      mkdir -p $OUTNAUGHT/$cycletype/$pole/$tag
    fi
  done
done


##################################################################
#Get required data files:
#Get the meteorological forcing data for the forecast and hindcast
#######################################################################
time  $EXDIR/getmet.jcl ${CC}$tag


#######################################################################
#Begin the hindcast/forecast cycle
#######################################################################
for cycletype in hcst fcst
do
  FBASE=${FNAUGHT}/$cycletype
  OUTBASE=${OUTNAUGHT}/$cycletype
  export FBASE OUTBASE 
#If we are making a hard restart, set up a restart file with initial ice 
#  thicknesses + concentrations
#Arg list is sst, conc, restart, concweight, sstweight, hthick
#  sst, conc = .TRUE. means to adjust these parameters according to input data
#  restart   = .TRUE. means run from restart file
#  concweight = weighting to give ice concentration analysis
#  sstweight  = weighting to give the sst analysis
#  hthick     = thickness to set new ice to 
if [ $HARDSTART = 'yes' ] ; then
  for pole in $polelist
  do
    echo About to try to run the ic checker, $pole
    $EXDIR/ic.sh .TRUE. .TRUE. .FALSE.  $ANUDGE 0.5 0.5
    cp $OUTBASE/$pole/restart.$pole $HINDBASE/$pole
  done
fi

for pole in ${polelist}
do
  OUTDIR=$OUTBASE/$pole
  export pole OUTDIR
  rm *
  #Make sure the seaice model is here
  if [ $cycletype = hcst ] ; then
    dttag=6
  else
    dttag=12
  fi
  if [ ! -s seaice.${pole}.$dttag ] ; then
    cp ${EXDIR}/seaice.${pole}.$dttag .
  fi
  
  # the f option means force the link.  i.e., do it even if the file
  #  already exists
  ln -sf  ${REFDIR}/${pole}/MASK .
  ln -sf  ${REFDIR}/${pole}/tsdeep .
  ln -sf  ${REFDIR}/${pole}/tsshal .
  ln -sf  ${REFDIR}/${pole}/bathy.$pole bathy

  #If the hindcast stage has run, use that.  Otherwise, run from the
  #  last restart file present here
  if [ -s $HINDBASE/$pole/$tag/restart.$pole ] ; then
    cp $HINDBASE/$pole/$tag/restart.$pole $OUTBASE/$pole
    echo working from restart file
   else
    echo Failed to find a hindcast restart file! 
    $EXDIR/ic.sh .TRUE. .TRUE. .FALSE.  0.875 0.5 0.6
    cp restart.$pole $OUTBASE/$pole
  fi  
  
  #Collect the forecast files
  if [ $cycletype = hcst ] ; then
    times=$hcsttimes
  else
    times=$fcsttimes
  fi
  for time in $times
  do

    #If there is not already a met file, create it.  If there is, link to it.
    #This needs to be reviewed and probably changed significantly
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

    #Establish the restart file for this run:
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
  
    #-------------------------------------------------------
    #Finally, conduct the forecast
    #Note the ddtag to distinguish between hindcast (6 hrs) and
    # forecast (12 hour dt) versions.  Inelegant.
    time ./seaice.${pole}.$dttag < runin
    gprof ./seaice.${pole}.$dttag >> ~/icemodel/icemodel/running/stats.$pole 
    #-------------------------------------------------------
    #Manage the output files
    #Fields for general use, to be overwritten
    cp conc     $OUTBASE/$pole/conc.$pole.$tag$time
    cp RESTARTn $OUTBASE/$pole/restart.$pole
    if [ $cycletype = hcst ] ; then
      mv RESTARTn $HINDBASE/$pole/restart.$pole  
    fi
  
    #Fields for archival
    FDIR=$OUTBASE/$pole/$tag
    tar c${TAROP}vf $FDIR/f$time.tar${TAROP} \
           thick conc vels atm.flux oce.flux hml sml tml FORT.16 FORT.18 
    rm     thick conc vels atm.flux oce.flux hml sml tml FORT.16 FORT.18

  done
  #above finishes run through time for a specific region

done
#finished all regions

done
#Finished all type of cycling (hindcast/forecast/things to be invented)
