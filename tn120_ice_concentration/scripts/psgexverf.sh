#!/bin/sh

#Making a 'unit test' on the polar stereographic gribbing
#Run twice -- prod versions, and test versions
#Robert Grumbine 25 Jul 2011

#set -xe

tag4=20110725
unit_src=/u/wx21rg/rgdev/ice_unit

#Make up the grib files for the polar stereographic data
echo $tag4 | cut -c1-2  > psin
echo $tag4 | cut -c3-4  >> psin
echo $tag4 | cut -c5-6  >> psin
echo $tag4 | cut -c7-8  >> psin

for type in prod test
do

  EXDIR=/nw${type}/exec/
  for pole in north south north12 south12
  do
    if [ -f fort.11 ] ; then
      rm fort.11
    fi
    if [ -f fort.51 ] ; then
      rm fort.51
    fi

    ln -sf $unit_src/seaice.t00z.umask${pole} fort.11
    ${EXDIR}/seaice_psg${pole} < psin
    if [ -f fort.51 ] ; then
      mv fort.51 ${type}_${pole}psg
    fi

  done
done
rm fort.11 fort.52

#Now see if the output is the same:
for pole in north south north12 south12
do
  cmp -l prod_${pole}psg test_${pole}psg
done
