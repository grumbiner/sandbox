#!/bin/sh

tag=20000101
while [ $tag -le 20001231 ]
do
  stag=`echo $tag | cut -c5-8`
  ln -sf low.$stag climo

  grads -lcb ref.gs
  for base in avg 
  do
    mv $base.gif ${base}_$stag.gif
  done

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`

done
