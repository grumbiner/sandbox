#!/bin/bash

base=$1
psgdir=/home/ftp/pub/ice/archive
uncompress $psgdir/?psg.${base}.Z

for pole in north south
do
  p=`echo $pole | cut -c1`
  wgrib ${psgdir}/${p}psg.$base > ${p}index
done

set -x

mo=`echo $base | cut -c3-4`
for dy in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 \
          21 22 23 24 25 26 27 28 29 30 31 
do
  if [ $dy -eq 31 -a \( \
       $mo -eq 02 -o $mo -eq 04 -o $mo -eq 06 -o $mo -eq 09 -o $mo -eq 11 \) ] ; \
  then
    #do nothing
    echo ' ' 
  else
  for pole in north south
  do
    p=`echo $pole | cut -c1`
    grep ${base}$dy ${p}index | wgrib -i ${psgdir}/${p}psg.$base -o ${p}$dy -nh 
    ~/execs/$pole.movie ${p}$dy 
    rm ${p}$dy
  done
  if [ -s n${dy}.bin -a -s s${dy}.bin ] ; then
    ~/execs/rtxpm n${dy}.bin /home/ftp/pub/ice/nland.map n${base}${dy}.xpm \
                  s${dy}.bin /home/ftp/pub/ice/sland.map s${base}${dy}.xpm
    rm n${dy}.bin s${dy}.bin
    loadwait xpmtogif n${base}${dy}.xpm
    loadwait xpmtogif s${base}${dy}.xpm
    rm *.xpm
  fi

  fi
done

for pole in north south
do
  p=`echo $pole | cut -c1`
  ls -1  ${p}${base}*.gif > alpha
  whirlgif -o ${p}anim.${base}.gif -i alpha
done

compress $psgdir/?psg.$base
