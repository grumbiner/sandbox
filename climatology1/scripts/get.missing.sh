#!/bin/sh

for f in ice.20080211 ice.20080428 ice.20080429 ice.20081106 ice.20090308 ice.20090403 
do
  tag=`echo $f | cut -c5-14`
  yy=`echo $tag | cut -c1-4`
  mm=`echo $tag | cut -c5-6`
  dd=`echo $tag | cut -c7-8`
  hsi get /NCEPPROD/hpssprod/runhistory/rh${yy}/${yy}${mm}/${yy}${mm}${dd}/com_omb_prod_sice.${tag}.tar
done
