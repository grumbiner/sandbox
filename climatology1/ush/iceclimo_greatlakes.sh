#!/bin/sh

#Great Lakes assumed (based on observations 1972-2014) to not be ice 
#  covered before 1 November, nor after 31 May.

#Produce (up to) two estimates for Great Lakes ice cover -- one purely from
#  climatology (1977-2006), and one conditional on the IMS ice
#  mask.  If outside the window for ice cover, return a zero field

#Assumes PDY, DCOM is set by calling script
#Assumes FIXDIR, EXDIR is set by calling script
#Assumes wgrib2 is in current working directory (or found by PATH)

tag=`echo $PDY | cut -c5-8`
if [ \( $tag -lt 531 \) -o \( $tag -ge 1101 \) ] ; then
  #May have an ice cover
  tar xf $FIXDIR/gliceclimatology.tgz count.$tag
  $EXDIR/noice_gl count.$tag ice1.$PDY
  #if [ -f $DCOM/us007003/$PDYm1/wgrbbul/imssnow96.grb.grib2 ] ; then
  #  cp $DCOM/us007003/$PDYm1/wgrbbul/imssnow96.grb.grib2 .
  if [ -f $DCOM/imssnow96.grb.grib2.$PDY ] ; then
    cp $DCOM/imssnow96.grb.grib2.$PDY imssnow96.grb.grib2
    wgrib2 imssnow96.grb.grib2 | grep ICEC | wgrib2 -i imssnow96.grb.grib2 -bin imsice
    $EXDIR/imsice_gl count.$tag imsice ice2.$PDY 
  fi 
else
  cp $FIXDIR/gl_zero ice1.$PDY
fi

#if ice2 is present, it should be better than ice1.  Else use ice1

#Robert Grumbine 10 March 2014
