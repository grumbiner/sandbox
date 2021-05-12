#!/bin/ksh
#Version computing with min_conc as argument

tag=20000225
while [ $tag -le 20040531 ]
do
  yy=`echo $tag | cut -c1-4`

  ./seas.x ${yy}/umasknorth12.$tag 15 > nh_area.$tag
  ./glob globs/fill5min.$tag 15 > glob_area.$tag

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
