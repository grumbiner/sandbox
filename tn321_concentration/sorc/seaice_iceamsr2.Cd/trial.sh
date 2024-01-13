#!/bin/bash

source ../../versions/run.ver

module load imagemagick

tag=20210410
while [ $tag -le 20230410 ]
do
  obs=$HOME/noscrub/seaice/developer/amsr2.$tag/amsr2.$tag

  if [ -f $obs ] ; then
    time ./seaice_iceamsr2 $HOME/noscrub/seaice/developer/amsr2.$tag/amsr2.$tag a b nhbase shbase nh_conc sh_conc gshhs distance > out.$tag
    rm nhbase_lr shbase_lr nhbase_hr shbase_hr
    mv n.xpm nh_$tag.xpm
    mv s.xpm sh_$tag.xpm
    convert nh_$tag.xpm nh_$tag.gif
    convert sh_$tag.xpm sh_$tag.gif
  fi

  tag=`expr $tag + 1`
  tag=`$HOME/bin/dtgfix3 $tag`
done

scp -p *.gif seaice@emcrzdm:polar/filters/amsr2/original

