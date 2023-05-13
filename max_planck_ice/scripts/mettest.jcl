# QSUB -o mettest.out
# QSUB -lT  150
# QSUB -lM    2.0MW
# QSUB -me

/bin/sh

tag=`date +'%y%m%d'`
RUNDIR=/tmp/wd21rg
EXDIR=/wd2/wd21/wd21rg/icemodel/meteo

FBASE=/com/mrf/PROD/mrf.${tag}/drfmr.T00Z.SFLUXGrbF
export tag RUNDIR EXDIR FBASE

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
  for time in 24 36 48 60 72 84 96 108 120 132 144 156 168 
  do
    i=`expr $i + 1`
    assign -a ${FBASE}${time} -s unblocked   fort.$i
  done

for pole in north 
do

  #Assign the output file for the translated fields
  assign -a metout.$pole           fort.20
  touch metin
  echo 1 > metin
  ja
    time ${EXDIR}/met.$pole < metin
  ja -chlst
done
rm metin
