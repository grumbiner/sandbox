#!/bin/sh

jday=31
yy=2001

echo /data/forecasts/ssmi.decoded > delta

time ./weather.x delta ../../../fix/nland ../../../fix/sland \
  n3ssmi.13 s3ssmi.13 umasknorth.13 umasksouth.13 \
  $jday $yy 246 > v.13
if [ ! -d product.x ] ; then
  mkdir product.x
fi
ref.output/toxpm_northc umasknorth.13 ../../../fix/nland n.xpm
mv *.13 count.? n.xpm product.x

#time ./weather.p15 delta ../../../fix/nland ../../../fix/sland \
#  n3ssmi.13 s3ssmi.13 umasknorth.13 umasksouth.13 \
#  $jday $yy 246 > v.13
#mkdir product15
#mv *.13 count.? product15

