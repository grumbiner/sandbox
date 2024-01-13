#Operational sea ice forecast model control script
#QSUB -o /wd2/wd21/wd21rg/runout/fullfore.out
#limits should be derived from resolution. note.
#QSUB -lT  300
#QSUB -lM    4.0Mw
#QSUB -me

#tag=`date +'%y%m%d'`
tag=960222
tagm=`expr $tag - 1 `
tagm=`dtgfix3 $tagm `
RUNDIR=/tmp/wd21rg
FBASE=/com/mrf/prod/mrf.${tag}/drfmr.T00Z.SFLUXGrb
EXDIR=/wd2/wd21/wd21rg/execs
REFDIR=/wd2/wd21/wd21rg/icemodel/running
OUTDIR=/ombptmp/ice/iceout2
HINDDIR=/eldisk1/wd21rg/iceout2
export tag RUNDIR REFDIR EXDIR OUTDIR FBASE HINDDIR

#Nothing below this point should need changing -------------------------

if [ ! -d $RUNDIR ] ; then
  mkdir $RUNDIR
fi
cd $RUNDIR

FILENV=.assign
export FILENV 

#set -xe
for pole in north south 
do
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
do
  i=40
  i=`expr $i + 1`
  assign -a ${FBASE}F${time} -s unblocked   fort.$i
  j=`expr $i + 30`
  assign -a ${FBASE}iF${time} -s unblocked   fort.$j

  #Assign the output file for the translated fields
  assign -a metout.$pole           fort.20
  touch metin
  echo 1 > metin
  ja
    time ${EXDIR}/met.$pole < metin
  ja -chlst
  rm metin

  cp metout.${pole} metout
  chmod 644 metout

  set -x
  if [ -s $OUTDIR/restart.$pole ] ; then
    if [ $time -eq 12 ] ; then
      $HOME3/icemodel/ic/ic.jcl .TRUE. .FALSE.
    else
      $HOME3/icemodel/ic/ic.jcl .FALSE. .FALSE.
    fi
    cp $OUTDIR/restart.$pole RESTARTo
    echo ${REFDIR}/${pole}/runparm.${pole} > runin
   else
    touch RESTARTo
    echo ${REFDIR}/${pole}/runzero.${pole} > runin
  fi

  ja
  time stossel.${pole} < runin
  ja -chlst

  rm runin

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
cd $TDIR 
if [ -s $OUTDIR/conc.$pole.${tag}168 ] ; then
  if [ -f $OUTDIR/conc.$pole.${tagm}168 ] ; then
    rm $OUTDIR/conc.$pole.${tagm}?? $OUTDIR/conc.$pole.${tagm}???
  fi
fi


done
#finished all regions

cd $OUTDIR
nohup time ./xpm &
nohup time $HOME3/iscore/opscore.jcl &
