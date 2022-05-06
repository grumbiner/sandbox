#!/bin/ksh

set -x

export utilexec=/nwprod/util/exec


tag=19960101
while [ $tag -le 19961231 ]
do

  PDY=`echo $tag | cut -c5-8`

  #########Grib1 construction and wmo file:
  #Engrib the 30' lat-long file
  #Put the 4 digit year date in file ein
  echo $tag > ein

  rm fort.*
  ln -sf low.$PDY             fort.11
  touch eng.$PDY
  ln -sf eng.$PDY              fort.51
  ln -sf wmoglobice.${PDY}.grb fort.52

  ./seaice_ice2grib < ein >> out 2>errfile

  #Engrib the 5' lat-long file
  #Put the 4 digit year date in file ein
  echo $tag > ein

  rm fort.*
  ln -sf high.$PDY             fort.11
  touch eng5min.$PDY
  ln -sf eng5min.$PDY              fort.51
#Note that there is no wmo file for high res global
  ./seaice_ice2grib5min< ein >> out 2>errfile

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`

done
