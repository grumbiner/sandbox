#!/bin/sh

#Run avhrr analysis up to the present

tag=20090209
tmp=`date +"%Y%m%d"`
tmp=`expr $tmp - 1`
tmp=`/u/wx21rg/bin/dtgfix3 $tmp`

while [ $tag -le $tmp ]
do
  time /u/wx21rg/rgdev/satellites/avhrr/others.sh $tag 
  tag=`expr $tag + 1`
  tag=`/u/wx21rg/bin/dtgfix3 $tag`
done

