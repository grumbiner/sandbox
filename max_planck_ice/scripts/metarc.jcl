# QSUB -o metarc.out
# QSUB -lT  149
# QSUB -lM  3.0MW
# QSUB -me

/bin/sh
FILENV=.assign
export FILENV
arcdir=/wd2/wd20/wd20yz/bin
export arcdir

cd /wd2/wd21/wd21rg/hibler/running
b=9308

#Extract files from the cray archive
for idate in 24 25 26 27 28 29 30 
do
  if [ ! -f crsflx.${b}${idate}00 ] ; then
    $arcdir/tarxvf  gz${b}$idate crsflx.${b}${idate}00 crsflx.${b}${idate}12
  fi
done

NCPUS=2
export NCPUS
i=50
for idate in 24 25 26 27 28 29 30
do
  i=`expr $i + 1`
  assign -a crsflx.${b}${idate}00  -s unblocked   fort.$i
  i=`expr $i + 1`
  assign -a crsflx.${b}${idate}12  -s unblocked   fort.$i
done

#Assign the output file for the translated fields
assign -a metout           fort.20

#execute the meteorological transform
ja
  touch metin
  echo 14 > metin
  if [ ! -f met ] ; then
    mv ../meteo/met ./met
  fi
  if [ -f ../meteo/met ] ; then
    mv ../meteo/met ./met
  fi
  time met < metin
ja -chlst
