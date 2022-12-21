# QSUB -o metoper.out
# QSUB -lT  149
# QSUB -lM    2.0MW
# QSUB -me

/bin/sh
FILENV=.assign
export FILENV
NCPUS=2
export NCPUS

cd /wd2/wd21/wd21rg/icemodel/running
EXDIR=/dm/wd21rg/execs

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

for pole in north south
do
#execute the meteorological transform
  #Assign the output file for the translated fields
  assign -a metout.$pole           fort.20
  touch metin
  echo 14 > metin
  time ${EXDIR}/met.$pole < metin
done
rm metin

ja -chlst
