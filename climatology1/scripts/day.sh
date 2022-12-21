#!/bin/sh

fn=`basename $1`
tag=`echo $fn | cut -f2 -d\.`
ln -sf ice.$tag day
#mkdir $tag
grads -lcb day.gs
for base in conc
do
  mv $base.gif ${base}_$tag.gif
done
