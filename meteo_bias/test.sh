#!/bin/sh

set -x

BASE=/usr2/rmg3/gdas.2006/
start=20060101 
stop=20061231
nday=7

#---------------- should need no changes below here

if [ -f out ] ; then
  rm out
fi

tag=$start
while [ $tag -le $stop ]
do
  count=1
  
  etag=$tag
  k=0
  while [ $k -lt $nday ]
  do
    etag=`expr $etag + 1`
    etag=`dtgfix3 $etag`
    k=`expr $k + 1`
  done
  if [ ! -f metout/out.$etag ] ; then
    while [ $tag -le $stop -a  $count -le nday ]
    do
      yy=`echo $tag | cut -c1-4`
      mm=`echo $tag | cut -c5-6`
    
      if [ ! -d ${BASE}/gdas.$tag ] ; then
        ret=`pwd`
        cd $BASE
        tar xvf ${mm}.tar
        cd $ret
      fi
    
      for cy in 00 06 12 18
      do
        for hh in 00 03
        do 
        fn=$BASE/gdas.$tag/gdas1.t${cy}z.sfluxgrbf$hh
    
        if [ -f $fn ] ; then
          wgrib $fn > index
          for parm in SHTFL LHTFL DLWRF:kpds5=205 \
                  ULWRF:kpds5=212:kpds6=1 \
                  USWRF:kpds5=211:kpds6=1 DSWRF:kpds5=204 ICEC \
                  GFLUX LAND ICETK SNOD PRATE
  #        for parm in TMP:kpds5=11:kpds6=1: 
  #                UGRD VGRD \
  #                SPFH PRES:kpds5=1:kpds6=1: HPBL ALBDO
          do
            grep $parm index | wgrib -i $fn -o metout/out.$etag -nh -append
          done
          #want to extract point of interest here for the met file in simple input
          #need an else clause to manage missing days
        fi
        done
    
      done
    
      tag=`expr $tag + 1`
      tag=`dtgfix3 $tag`
      count=`expr $count + 1`
    done
  else
    tag=$etag
  fi
  
  
  if [ ! -f accumulation ] ; then
    time ./newnet metout/out.$etag > alpha.$tag
  else
    time ./newnet metout/out.$etag accumulation > alpha.$tag
  fi

  #mv metout/out metout/out.$tag
  mv freezing.xpm freezing_$tag.xpm
  mv melting.xpm melting_$tag.xpm
  mv heating.xpm heating_$tag.xpm
  cp accumulation metout/acc.$tag
  count=1
done
