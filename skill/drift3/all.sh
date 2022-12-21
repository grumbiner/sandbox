#!/bin/bash

tag=20000101
while [ $tag -le 20041231 ]
do
  y2=`echo $tag | cut -c3-8`
  if [ ! -f sk2.1$y2 -a -f sk2.$tag ] ; then
    ln -s sk2.$tag sk2.1$y2
  fi
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
