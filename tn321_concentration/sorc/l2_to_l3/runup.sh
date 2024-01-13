#!/bin/sh
export DATA=~/noscrub/l2
export FIX=~/rgdev/concentration/fix
export EXEC=~/rgdev/concentration/exec

module load ips/19.0.5.281
module load NetCDF/4.5.0

yy=04
tag=20${yy}0101
while [ $tag -le 20${yy}1231 ]
do
  echo $tag

  if [ -f ${DATA}/l2.ssmi.${tag}.nc ] ; then
    ~/rgdev/concentration/sorc/l2_to_l3/ssmi_tol3 ${DATA}/l2.ssmi.${tag}.nc nmap.${tag}.f15 smap.${tag}.f15
    ${EXEC}/seaice_north12xpm nmap.${tag}.f15 ${FIX}/seaice_nland127.map nmap.${tag}.f15.xpm
    ${EXEC}/seaice_south12xpm smap.${tag}.f15 ${FIX}/seaice_sland127.map smap.${tag}.f15.xpm
  fi

  if [ -f ${DATA}/l2.f285.51.${tag}.nc ] ; then
    ~/rgdev/concentration/sorc/l2_to_l3/ssmis_tol3 ${DATA}/l2.f285.51.${tag}.nc nmap.${tag}.f17 smap.${tag}.f17
    ${EXEC}/seaice_north12xpm nmap.${tag}.f17 ${FIX}/seaice_nland127.map nmap.${tag}.f17.xpm
    ${EXEC}/seaice_south12xpm smap.${tag}.f17 ${FIX}/seaice_sland127.map smap.${tag}.f17.xpm
  fi

  if [ -f ${DATA}/l2.f286.52.${tag}.nc ] ; then
    ~/rgdev/concentration/sorc/l2_to_l3/ssmis_tol3 ${DATA}/l2.f286.52.${tag}.nc nmap.${tag}.f18 smap.${tag}.f18
    ${EXEC}/seaice_north12xpm nmap.${tag}.f18 ${FIX}/seaice_nland127.map nmap.${tag}.f18.xpm
    ${EXEC}/seaice_south12xpm smap.${tag}.f18 ${FIX}/seaice_sland127.map smap.${tag}.f18.xpm
  fi

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
