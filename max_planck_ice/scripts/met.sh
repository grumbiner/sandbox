#!/bin/ksh
#Script to use wgrib to extract fields required by the ice model
#  from the MRF surface flux files, and then create a meteorological
#  input file for a model specified by the environment variable pole
#datafile is an argument now
#pole must be exported by calling script
#Robert Grumbine
#Last Modified 20 February 1997

#Environment variables:
#  pole - Defines which model to use
#  EXDIR - location of executables wgrib, met.$pole
#Arguments:
#  First argument is name of the grib file to process
pole=${pole:-north}
EXDIR=${EXDIR:-/usr1/rmg3/icemodel/execs}

#set -x
datafile=$1
i=50

${EXDIR}/wgrib $datafile > index 

#Note that 71, 214 gives low cloudiness.  In the more recent record,
#71,200 gives total cloud.  Since the model no longer uses cloudiness,
#use either.
for field in kpds5=1:kpds6=1: \
  kpds5=33:kpds6=105: \
  kpds5=34:kpds6=105: \
  kpds5=51:kpds6=105: \
  kpds5=59:kpds6=1: \
  kpds5=71:kpds6=214: \
  kpds5=81:kpds6=1: \
  kpds5=11:kpds6=1: \
  kpds5=204:kpds6=1: \
  kpds5=205:kpds6=1: \
  kpds5=212:kpds6=1: \
  kpds5=11:kpds6=105

do
  grep $field index | $EXDIR/wgrib $datafile -i -o fort.$i
  echo $?
  i=`expr $i + 1`
done

echo 1 > metin
time $EXDIR/met.$pole < metin
mv fort.20 metout.$pole
