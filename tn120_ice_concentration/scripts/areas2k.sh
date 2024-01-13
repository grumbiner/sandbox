#!/bin/sh

start=20000101
cur=$start
end=20001231

while [ $cur -le $end ] 
do
  yy=`echo $cur | cut -c1-4`
  if [ ! -f index.$yy ] ; then
    wgrib ice.$yy > index.$yy
  fi
  t=`echo $cur | cut -c3-90`
  t=${t}00
  grep $t index.$yy | wgrib -i ice.$yy -o dy -nh
  ./areas dy gland $cur 
  cur=`expr $cur + 1`
  cur=`dtgfix3 $cur`
done
