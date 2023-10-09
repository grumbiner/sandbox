#!/bin/ksh

#Use IMS ice analysis and conditional climatology to estimate current
#  ice cover
#Robert Grumbine 10 July 2014

#must get DCOMm1, PDY, FIXseaice_concentration_analysis, EXECseaice_concentration_analysis from environment
#ims is PDYm1 -- it doesn't run until ~12 hours after the sea ice concentration analysis
echo $DCOMm1 $PDY $PDYm1 $FIXseaice_concentration_analysis $EXECseaice_concentration_analysis

stag=`echo $PDY | cut -c5-8`

tar xf  $FIXseaice_concentration_analysis/counts.tgz count.$stag

if [ -f $DCOMm1/wgrbbul/imssnow96.grb.grib2 ] ; then
  cp $DCOMm1/wgrbbul/imssnow96.grb.grib2 imssnow96.grb.grib2
  ${EXECutil}/wgrib2 imssnow96.grb.grib2 | grep ICEC | ${EXECutil}/wgrib2 -i imssnow96.grb.grib2 -bin imsice.bin
  $EXECseaice_concentration_analysis/imsice count.$stag imsice.bin imsice.$PDY
    export err=$?;err_chk
else
  echo could not find imssnow96.grb.grib2 in ${DCOMm1}/wgrbbul
fi

set +xa
