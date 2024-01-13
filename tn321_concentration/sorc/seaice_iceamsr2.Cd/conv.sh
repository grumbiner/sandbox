#!/bin/bash

source ../../versions/run.ver

module load imagemagick

tag=20201222
while [ $tag -le 20210410 ]
#while [ $tag -le 20230410 ]
do
  obs=$HOME/noscrub/seaice/developer/amsr2.$tag/amsr2.$tag

  if [ -f $obs ] ; then
    convert nh_$tag.xpm nh_$tag.gif
    convert sh_$tag.xpm sh_$tag.gif
  fi

  tag=`expr $tag + 1`
  tag=`$HOME/bin/dtgfix3 $tag`
done

scp -p *.gif seaice@emcrzdm:polar/filters/amsr2/original

