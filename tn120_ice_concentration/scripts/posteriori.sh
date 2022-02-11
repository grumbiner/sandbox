#!/bin/sh

#get the posteriori flag file
#get the nh high res grids
#get the sh high res grids
#get the sst for the day (rey archive)
start=20040901
end=20090831

base=/usr2/rmg3/data2/3d/concentrations/ncepice/

#--------------- Should need no changes below here --------------
tag=$start
while [ $tag -le $end ]
do
  ym=`echo $tag | cut -c1-6`
  if [ ! -f nh$ym.index ] ; then
    wgrib $base/north/n12psg.$ym > nh$ym.index
  fi
  if [ ! -f sh$ym.index ] ; then
    wgrib $base/south/s12psg.$ym > sh$ym.index
  fi
  last=`echo $tag | cut -c3-8`
  if [ -f nh ] ; then 
     rm nh ; fi
  if [ -f sh ] ; then 
     rm sh ; fi
  grep $last nh$ym.index | wgrib -i $base/north/n12psg.$ym -nh -o nh > /dev/null 2> /dev/null
  grep $last sh$ym.index | wgrib -i $base/south/s12psg.$ym -nh -o sh > /dev/null 2> /dev/null
  if [ ! -s nh -o ! -s sh ] ; then
    echo no data for $tag 
  else
    x=`./posteriori.x flags nh sh`
    echo $x $tag
  fi

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
  
#cycle for all dates 
#  compute area filtered by day, posteriori 
#  repeat with that day's ssts
