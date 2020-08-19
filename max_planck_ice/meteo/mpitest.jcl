# QSUB -o hires.out
# QSUB -lT  150
# QSUB -lM    8.0MW
# QSUB -me

/bin/sh

tag=`date +'%y%m%d'`
RUNDIR=/tmp/wd21rg
EXDIR=/wd2/wd21/wd21rg/execs
FBASE=/com/mrf/PROD/mrf.${tag}/drfmr.T00Z.SFLUXGrbF
REFDIR=/wd2/wd21/wd21rg/icemodel/running
OUTDIR=/wd2/wd21/wd21rg/icefore
export tag RUNDIR REFDIR EXDIR OUTDIR FBASE

if [ ! -d $RUNDIR ] ; then
  mkdir $RUNDIR
fi
cd $RUNDIR

FILENV=.assign
NCPUS=2
export FILENV NCPUS

#Collect the forecast files
#Attach files to unit numbers
  i=50
  for time in 12 24 36 48 60 72 84 96 108 120 132 144 156 168 
  do
    i=`expr $i + 1`
    assign -a ${FBASE}${time} -s unblocked   fort.$i
  done

for pole in north south
do

  #Assign the output file for the translated fields
  assign -a metout.$pole           fort.20
  touch metin
  echo 14 > metin
  ja
#    time ${EXDIR}/met.$pole < metin
  ja -chlst
done
rm metin


#Now collect the forecast files:
NCPUS=1
export NCPUS
 
for pole in north south
do

#Make sure the stossel model is here
  cp ${EXDIR}/stossel.${pole} .
  cp ${REFDIR}/${pole}/MASK .
  cp ${REFDIR}/${pole}/tsdeep .
  cp ${REFDIR}/${pole}/tsshal .

  cp metout.${pole} metout
  chmod 644 metout

  if [ -f $OUTDIR/restart.$pole ] ; then
    cp $OUTDIR/restart.$pole RESTARTo
    echo ${REFDIR}/${pole}/runparm.${pole} > runin
   else
    touch RESTARTo
    echo ${REFDIR}/${pole}/runzero.${pole} > runin
  fi

  rm RESTARTn thick conc vels ?ml ???.flux FORT.1[68]
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
  FDIR=$OUTDIR/$pole
  if [ ! -d $FDIR ] ; then
    if [ ! -d $OUTDIR/$pole ] ; then
      mkdir $OUTDIR/$pole
    fi
    mkdir $FDIR
  fi
#  tar -cvf ${FDIR}/${tag}.tar \
#     RESTARTn \
#     thick    \
#     conc     \
#     vels     \
#     atm.flux \
#     oce.flux \
#     hml      \
#     sml      \
#     tml      \
#     FORT.16  \
#     FORT.18  
  cp FORT.18 ${FDIR}/fort18.$tag

done
