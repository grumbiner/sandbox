#!/bin/sh

tag=20010101
#while [ $tag -le 20010105 ]
while [ $tag -le 20011231 ]
do
  for hh in 00 06 12 18
  do
    if [ -f flxf06.${tag}$hh ] ; then
      wgrib flxf06.${tag}$hh | wgrib -i flxf06.${tag}$hh -nh -o metout
      ./summet metout prevout sumout
     else
      echo Could not find flxf06.${tag}$hh
    fi
    mv sumout prevout
  done

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
./avgmet prevout avgout > avg.txt
  
