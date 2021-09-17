# QSUB -o icefore.out
# QSUB -lT   30
# QSUB -lM    2.0MW
# QSUB -me


RUNDIR=/wd2/wd21/wd21rg/icemodel/running
cd $RUNDIR
EXDIR=/dm/wd21rg/execs

NCPUS=1
export NCPUS
 
for POLE in north south
do

#Make sure the stossel model is here
cp ${EXDIR}/stossel.${POLE} .
cp ${POLE}/MASK .
cp ${POLE}/tsdeep .
cp ${POLE}/tsshal .

cp metout.${POLE} metout
chmod 644 metout
touch RESTARTo

echo ${RUNDIR}/${POLE}/runzero.${POLE} > runin
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

done
