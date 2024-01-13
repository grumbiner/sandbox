#!/bin/ksh

#Use IMS ice analysis and conditional climatology to estimate current
#  ice cover
#Robert Grumbine 10 July 2014

#must get PDY, FIXsice, EXECsice from environment
#DCOM=/dcom/us007003/$PDYm1/wgrbbul/
DCOM=ims/20150308/wgrbbul
stag=`echo $PDY | cut -c5-8`

tar xf  $FIXsice/counts.tgz count.$stag

cp $DCOM/imssnow96.grb.grib2 imssnow96.grb.grib2
wgrib2 imssnow96.grb.grib2 | grep ICEC | wgrib2 -i imssnow96.grb.grib2 -bin imsice.bin
./imsice count.$stag imsice.bin imsice.$PDY
