#!/bin/sh
#Forecast, requires that the following be set in the environment:
# RUNDIR, FBASE, EXDIR, REFDIR, OUTBASE, HINDBASE, SSDIR, tag


#Nothing below this point should need changing -------------------------
if [ -z $RUNDIR ] ; then
  echo No run directory specified!
  exit -1
fi
if [ -z $FBASE ] ; then
  echo No meteo data directory specified
  exit -1
fi
if [ -z $EXDIR ] ; then
  echo No executables directory specified
  exit -1
fi
if [ -z $REFDIR ] ; then
  echo No data reference directory specified
  exit -1
fi
if [ -z $OUTBASE ] ; then
  echo No directory named for output
  exit -1
fi
if [ -z $HINDBASE ] ; then
  echo No hindcast directory named -- needed for restart file
  exit -1 
fi
if [ -z $SSDIR ] ; then
  echo No directory named for SSMI-ice concentrations and SST
  exit -1
fi
if [ -z $tag ] ; then
  echo No date specified to run forecast for
  exit -1
fi 

delt=12
export delt
#Note that for operations, ic.jcl and met.jcl must be inlined
set -x

tagm=`expr $tag - 1`
tagm=`dtgfix3 $tagm`
mm=`echo $tag | cut -c3-4`
export tagm mm 

#Ensure that the named directories exist
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

cd $FBASE
echo `time  $EXDIR/getmet.jcl` getmet.jcl
for pole in south north 
do
  cd $RUNDIR
  OUTDIR=$OUTBASE/$pole
  export pole OUTDIR
  rm *
#Make sure the ice model is here
  if [ ! -f stossel.${pole}.$delt ] ; then
    cp ${EXDIR}/stossel.${pole}.$delt .
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
    $EXDIR/ic.jcl .FALSE. .FALSE. .FALSE.  0.8 0.4 0.6
    cp restart.$pole $OUTBASE/$pole
  fi  

  #Collect the forecast files
  #Attach files to unit numbers
  for time in 12 24 36 48 60 72 84 96 108 120 132 144 156 168 \
             180 192 204 216 228 240 252 264 276 288 300 312 324 \
             336 348 360 372 384
  #for time in 00 06 12 18
  do

  # if there isn't already a met file, create it.  If there is, link to it.
    if [ ! -f ${FBASE}/met.${pole}.${tag}${time} ] ; then
      time ${EXDIR}/met.jcl ${FBASE}/drfmr.T00Z.SFLUXGrbF$time
      cp metout.${pole} ${FBASE}/met.${pole}.${tag}${time}
      ln -sf ${FBASE}/met.${pole}.${tag}$time metout
    else
      if [ -f metout ] ; then
        rm metout
      fi
      ln -sf ${FBASE}/met.${pole}.${tag}$time metout
    fi
  
    chmod 644 metout
  
    if [ -s $OUTBASE/$pole/restart.$pole ] ; then
      # Order is: concentration, SST, Restart file present
      if [ $time -eq 00 ] ; then
         $EXDIR/ic.jcl .FALSE. .FALSE.  .TRUE.  0.8 0.5 0.1
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
    time stossel.${pole}.$delt < runin
  
  #Fields for general use, to be overwritten
    cp conc     $OUTBASE/$pole/conc.$pole.$tag$time
    cp RESTARTn $OUTBASE/$pole/restart.$pole
    cp RESTARTn $OUTBASE/$pole/$tag/restart.$pole
  #  if [ $time -eq 24 ] ; then
  #    cp RESTARTn $HINDBASE/$pole/restart.$pole
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
  if [ -s $OUTBASE/$pole/conc.$pole.${tag}384 ] ; then
    if [ -f $OUTBASE/$pole/conc.$pole.${tagm}384 ] ; then
      rm $OUTBASE/$pole/conc.$pole.${tagm}?? 
      rm $OUTBASE/$pole/conc.$pole.${tagm}???
    fi
    #${EXDIR}/www.jcl
  fi

  #cd $FDIR
  #tar cvf t$tag.$pole.tar f*.tar
  #rm f*.tar
  
done
  
