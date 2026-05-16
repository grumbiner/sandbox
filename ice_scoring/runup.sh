#!/bin/sh
#Plot all forecast leads from a date

export tag=20181001
end=`date +"%Y%m%d"`

while [ $tag -le $end ]
do
  if [ -f scoreout.RTOFS.${tag}.192 ] ; then 
    ./plot.RTOFS.sh
  fi

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag `
done
