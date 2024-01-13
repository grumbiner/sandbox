# QSUB -o fullfore.out
# QSUB -lT  150
# QSUB -lM    8.0MW
# QSUB -me

/bin/sh

#set -x

tag=`date +'%y%m%d'`
RUNDIR=/tmp/wd21rg
EXDIR=/wd2/wd21/wd21rg/execs
FBASE=/com/mrf/PROD/mrf.${tag}/drfmr.T00Z.SFLUXGrb
REFDIR=/wd2/wd21/wd21rg/icemodel/running
OUTDIR=/wd2/wd21/wd21rg/icefore
pole=glk
export tag pole RUNDIR REFDIR EXDIR OUTDIR FBASE

if [ ! -d $RUNDIR ] ; then
  mkdir $RUNDIR
fi
cd $RUNDIR

FILENV=.assign
NCPUS=2
export FILENV NCPUS

#Make sure the stossel model is here
  cp ${EXDIR}/stossel.${pole} .
  cp ${REFDIR}/${pole}/MASK .
  cp ${REFDIR}/${pole}/tsdeep .
  cp ${REFDIR}/${pole}/tsshal .

#Collect the forecast files
#Attach files to unit numbers
for time in 12 24 36 48 60 72 84 96 108 120 132 144 156 168 
#for time in 12 
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

  if [ -f $OUTDIR/restart.$pole ] ; then
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
  cp RESTARTn $OUTDIR/restart.$pole
  cp thick    $OUTDIR/thick.$pole
  cp conc     $OUTDIR/conc.$pole
  cp vels     $OUTDIR/vels.$pole
  cp FORT.18  $OUTDIR/fort18.$pole

#Create/update the date file
  $REFDIR/datewk $tag
  mv date $OUTDIR/date.$pole

#Fields for archival
  if [ ! -d $OUTDIR/$pole ] ; then
    mkdir $OUTDIR/$pole
  fi
  FDIR=$OUTDIR/$pole/$tag
  if [ ! -d $FDIR ] ; then
    mkdir $FDIR
  fi
  mv RESTARTn $FDIR
  mv thick    $FDIR
  mv conc     $FDIR
  mv vels     $FDIR
  mv atm.flux $FDIR
  mv oce.flux $FDIR
  mv hml      $FDIR
  mv sml      $FDIR
  mv tml      $FDIR
  mv FORT.16  $FDIR
  mv FORT.18  $FDIR

done
