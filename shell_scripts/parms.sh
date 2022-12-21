#!/bin/sh
fhr=009
if [ ! -f pgrb2if${fhr} ] ; then
  wgrib2 pgrb2f$fhr > pgrb2if${fhr}
fi
iunit=11
 
for field in 'TMP:2 m above' 'RH:2 m above' 'CLWMR:1000 mb' PRATE CPRAT PWAT CRAIN CFRZR CICEP CSNOW PRES:surface TMP:surface ICEC LAND 
do
  grep "$field" pgrb2if$fhr | wgrib2 -i pgrb2f$fhr -append -order we:ns -bin fort.$iunit
done
