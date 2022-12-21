#!/bin/sh

#Construct global ice climatology fields

#Produce (up to) two estimates for ice cover -- one purely from
#  climatology (1981-2010), and one conditional on the IMS ice
#  mask.  
#if ice2 is present, it should be better than ice1.  Else use ice1

#Robert Grumbine 10 March 2014

#Assumes PDY, DCOM is set by calling script
#Assumes FIXDIR, EXDIR is set by calling script
#Assumes wgrib2 is in current working directory (or found by PATH)

tag=`echo $PDY | cut -c5-8`

tar xf $FIXDIR/global_counts.tgz count.$tag
$EXDIR/noice count.$tag ice1.$PDY

if [ -f $DCOM/imssnow96.grb.grib2.$PDY ] ; then
  cp $DCOM/imssnow96.grb.grib2.$PDY imssnow96.grb.grib2
  wgrib2 imssnow96.grb.grib2 | grep ICEC | wgrib2 -i imssnow96.grb.grib2 -bin imsice
  $EXDIR/imsice count.$tag imsice ice2.$PDY 
fi 
