#!/bin/bash

datafile=/home/ftp/pub/data0/flxf06.96090900
#datafile=/home/ftp/pub/cfs/etasrf/E29sf.grb
i=50
for field in kpds5=1:kpds6=1: \
  kpds5=33:kpds6=105: \
  kpds5=34:kpds6=105: \
  kpds5=51:kpds6=105: \
  kpds5=59:kpds6=1: \
  kpds5=71:kpds6=200: \
  kpds5=81:kpds6=1: \
  kpds5=11:kpds6=1: \
  kpds5=204:kpds6=1: \
  kpds5=205:kpds6=1: \
  kpds5=212:kpds6=1: \
  kpds5=11:kpds6=105

do
  grep $field index | wgrib $datafile -i -o fort.$i
#  grep $field E29.index | wgrib $datafile -i -o fort.$i
  echo $?
  i=`expr $i + 1`
done

echo 1 > metin
time ./met.glk < metin
