# QSUB -o premet.out
# QSUB -lT  150
# QSUB -lM    2.0MW
# QSUB -me

RUNDIR=/ptmp2/wd21rg
EXDIR=$HOME3/execs
REFDIR=/wd2/wd21/wd21rg/icemodel/running
OUTDIR=/ptmp2/wd21rg

export RUNDIR REFDIR EXDIR OUTDIR 

if [ ! -d $RUNDIR ] ; then
  mkdir $RUNDIR
fi
cd $RUNDIR

FILENV=.assign
NCPUS=2
export FILENV NCPUS

set -x
#loop over each region
base=9312
#tar xvf /ptmp1/wd21rg/met.${base}.tar
for pole in glk
do
#Make sure the stossel model is here
  cp ${EXDIR}/stossel.${pole} .
  cp ${REFDIR}/${pole}/MASK .
  cp ${REFDIR}/${pole}/tsdeep .
  cp ${REFDIR}/${pole}/tsshal .
  cp ${REFDIR}/${pole}/bathy  .

#Collect the forecast files, one at a time
#Attach files to unit numbers
#  for dy in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 \
#            19 20 21 22 23 24 25 26 27 28 29 30 31
  for dy in 01 
  do
  day=${base}${dy}
  for hh in 00 12 
  do

    time=$day$hh

#Now conduct the forecast:

    cp met.${pole}.$time metout
    chmod 644 metout

    if [ -s $OUTDIR/restart.$pole ] ; then
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
    cp RESTARTn $OUTDIR/restart.$pole.$time
    cp thick    $OUTDIR/thick.$pole.$time
    cp conc     $OUTDIR/conc.$pole.$time
    cp vels     $OUTDIR/vels.$pole.$time
    cp FORT.18  $OUTDIR/fort18.$pole.$time
  
#Fields for archival
    FDIR=$OUTDIR/$pole
    if [ ! -d $FDIR ] ; then
      if [ ! -d $OUTDIR/$pole ] ; then
        mkdir $OUTDIR/$pole
      fi
      mkdir $FDIR
    fi
    tar -cvf ${FDIR}/${time}.tar \
       RESTARTn \
       thick    \
       conc     \
       vels     \
       atm.flux \
       oce.flux \
       hml      \
       sml      \
       tml      \
       FORT.16  \
       FORT.18  
    cp FORT.18 ${FDIR}/fort18.$time
  
    mv metout.$pole met.$pole.$time

  done #hours

  if [ ! -f met.$pole.tar ] ; then
    tar cvf met.$pole.tar met.$pole.${time}
  else
    tar rvf met.$pole.tar met.$pole.${time}
  fi

  done #days

cp met.$pole.tar $DHOME

done #regions
