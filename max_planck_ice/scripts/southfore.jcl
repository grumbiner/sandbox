# QSUB -o fullfore.out
# QSUB -lT  120
# QSUB -lM    2.0MW
# QSUB -me

/bin/sh

RUNDIR=/tmp/wd21rg
REFDIR=/wd2/wd21/wd21rg/icemodel/running
EXDIR=/dm/wd21rg/execs
OUTDIR=/dm/wd21rg/icefore
export RUNDIR REFDIR EXDIR OUTDIR

cd $RUNDIR

FILENV=.assign
NCPUS=2
export FILENV NCPUS


#Collect the forecast files

tag=`date +'%y%m%d'`
FBASE=/com/mrf/PROD/mrf.${tag}/drfmr.T00Z.SFLUXGrbF
#Attach files to unit numbers
  i=50
  for time in 12 24 36 48 60 72 84 96 108 120 132 144 156 168 
  do
    i=`expr $i + 1`
    assign -a ${FBASE}${time} -s unblocked   fort.$i
  done

ja

for pole in south
do
  #Assign the output file for the translated fields
  assign -a metout.$pole           fort.20
  touch metin
  echo 14 > metin
  time ${EXDIR}/met.$pole < metin
done
rm metin

ja -chlst


#Now collect the forecast files:
NCPUS=1
export NCPUS
 
for POLE in south
do

#Make sure the stossel model is here
cp ${EXDIR}/stossel.${POLE} .
cp ${REFDIR}/${POLE}/MASK .
cp ${REFDIR}/${POLE}/tsdeep .
cp ${REFDIR}/${POLE}/tsshal .

cp metout.${POLE} metout
chmod 644 metout

if [ -f $OUTDIR/restart.$POLE ] ; then
  cp $OUTDIR/restart.$POLE RESTARTo
  echo ${REFDIR}/${POLE}/runparm.${POLE} > runin
 else
  touch RESTARTo
  echo ${REFDIR}/${POLE}/runzero.${POLE} > runin
fi

ja
time stossel.${POLE} < runin
ja -chlst

rm runin
mv RESTARTn restart.${POLE}
mv thick    thick.$POLE
mv conc     conc.${POLE}
mv vels     vels.${POLE}
mv atm.flux atm.flux.${POLE}
mv oce.flux oce.flux.${POLE}
mv hml      hml.${POLE}
mv sml      sml.${POLE}
mv tml      tml.${POLE} 
mv FORT.16  fort16.${POLE}
mv FORT.18  fort18.${POLE}
#Need to create/update the date file
#Also need to test that the run was successful before overwriting
#  the files which already exist in the forecast directory.
cp restart.${POLE} thick.$POLE conc.$POLE vels.$POLE ???.flux.$POLE \
   ?ml.$POLE fort1?.$POLE \
   $OUTDIR

done
