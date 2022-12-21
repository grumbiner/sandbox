#!/bin/sh

base=../daily.2e
tag=19810101
while [ $tag -le 20071031 ]
do
  if [ ! -f ice.$tag ] ; then
    echo identical for $tag
   else
    mv ~/current/ice.climo/data/ice.$tag ~/current/ice.climo/data/old
    mv ice.$tag ~/current/ice.climo/data
  fi
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
