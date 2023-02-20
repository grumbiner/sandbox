#!/bin/sh

tag=${tag:-`date +"%Y%m%d"`}
#tag=20130801
base=/com/omb/prod/

#Source:
suser=Robert.Grumbine
source1=prodwcoss
source2=devwcoss

#Target:
tuser=seaice
www_target=emcrzdm:/home/www/polar/seaice
ftp_target=emcrzdm:/home/ftp/polar/ice/
filebase=seaice.t00z.

#System:
#for source in $source1 $source2
for source in $source1 
do

  for f in sh.gif sh12.gif nh.gif nh12.gif global.gif global5min.gif
  do
#    scp -p $suser@$source:$base/sice.$tag/${filebase}$f $tuser@$www_target/$f 2> /dev/null
    scp -p $suser@$source:$base/sice.$tag/${filebase}$f $tuser@$www_target/analysis/$f 2> /dev/null
    scp -p $suser@$source:$base/sice.$tag/${filebase}$f $tuser@$ftp_target/$f 2> /dev/null
  done
  
  for f in southpsg.grib2 southpsg south12psg.grib2 south12psg northpsg.grib2 northpsg north12psg.grib2 north12psg
  do 
    scp -p $suser@$source:$base/sice.$tag/${filebase}$f $tuser@$ftp_target/$f.$tag 2> /dev/null
  done
  scp -p $suser@$source:$base/sice.$tag/*.kml $tuser@$ftp_target 2> /dev/null
  scp -p $suser@$source:$base/sice.$tag/*.kml $tuser@$www_target/analysis 2> /dev/null

#grb.grib2
#grb
#age
#5min.grb.grib2
#5min.grb
  scp -p $suser@$source:$base/sice.$tag/${filebase}grb.grib2 $tuser@$ftp_target/../cdas/eng.grib2.$tag 2> /dev/null
  scp -p $suser@$source:$base/sice.$tag/${filebase}grb $tuser@$ftp_target/../cdas/eng.$tag 2> /dev/null
  scp -p $suser@$source:$base/sice.$tag/${filebase}age $tuser@$ftp_target/../cdas/age.$tag 2> /dev/null
  scp -p $suser@$source:$base/sice.$tag/${filebase}5min.grb.grib2 $tuser@$ftp_target/../cdas/eng5min.grib2.$tag 2> /dev/null
  scp -p $suser@$source:$base/sice.$tag/${filebase}5min.grb $tuser@$ftp_target/../cdas/eng5min.$tag 2> /dev/null


# for the cgi:
  scp -p $suser@$source:$base/sice.$tag/${filebase}umasksouth12 $tuser@$www_target/south12 2> /dev/null
  scp -p $suser@$source:$base/sice.$tag/${filebase}umasksouth   $tuser@$www_target/south 2> /dev/null
  scp -p $suser@$source:$base/sice.$tag/${filebase}umasknorth12 $tuser@$www_target/north12 2> /dev/null
  scp -p $suser@$source:$base/sice.$tag/${filebase}umasknorth   $tuser@$www_target/north 2> /dev/null
  
done
