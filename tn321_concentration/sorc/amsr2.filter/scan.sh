#!/bin/sh
#

tag=20220101
while [ $tag -le 20230331 ]
do
  if [ ! -f p.$tag ] ; then
    if [ -f amsr2.$tag/perfect0.$tag ] ; then 
      python3 browse.py amsr2.$tag/perfect0.$tag | sort -n -k 7 > p.$tag
    fi
  fi

  if [ ! -f dr.$tag ] ; then
    if [ -f amsr2.$tag/perfect0.$tag ] ; then 
      python3 drbrowse.py amsr2.$tag/perfect0.$tag | sort -n -k 8 > dr.$tag
    fi
  fi

  tag=`expr $tag + 1`
  tag=`$HOME/bin/dtgfix3 $tag`
done
