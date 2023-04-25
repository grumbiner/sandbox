#!/bin/sh

#Get files from the assorted NMC models which have sea ice
#  of some sort, or should, in their output

#set -xe

log=wx21rg@cirrus:

#Date Management -- use yesterday because RTOFS-global doesn't finish until
#  ca. 1900 local.
if [ $# -ne 1 ] ; then
  tag=`date +"%Y%m%d"`
  tag=`expr $tag - 1`
  tag=`dtgfix3 $tag`
else
  tag=$1
fi

mkdir $tag
cd $tag

tag1=`expr $tag  + 1`; tag1=`dtgfix3 $tag1`
tag2=`expr $tag1 + 1`; tag2=`dtgfix3 $tag2`
tag3=`expr $tag2 + 1`; tag3=`dtgfix3 $tag3`
tag4=`expr $tag3 + 1`; tag4=`dtgfix3 $tag4`
tag5=`expr $tag4 + 1`; tag5=`dtgfix3 $tag5`
j=5
tagn=$tag5
while [ $j -lt 16 ]
do
  tagn=`expr $tagn + 1`; tagn=`dtgfix3 $tagn`
  j=`expr $j + 1`
done
tag16=$tagn

while [ $j -lt 30 ]
do
  tagn=`expr $tagn + 1`; tagn=`dtgfix3 $tagn`
  j=`expr $j + 1`
done
tag30=$tagn


#RTOFS-Global
CCSDIR=/com/rtofs/prod/rtofs.$tag
#Forcing files .a, .b
#  airtmp, precip, presur, radflx, shwflx, surtmp, tauewd, taunwd, vapmix, wndspd
#     rtofs_glo.anal.t00z, rtofs_glo.fcst1.t00z, fcst2, fcst3
# rtofs_glo_2ds_f003_3hrly_diag.nc (and _prog, by 3 hours from 3-144)
# rtofs_glo_2ds_n003_3hrly_diag.nc (and _prog, by 3 hours from 3-48)
# rtofs_glo_3dz_f006_6hrly_reg1.nc (and _reg2, _reg3, 6 hours from 6-144)
# rtofs_glo_3dz_n006_6hrly_reg1.nc (and _reg2, _reg3, 6 hours from 6-48)
#Note that f120 is not available until ca 1900 local.
for hrs in n048 f024 f120 
do
  scp -p ${log}${CCSDIR}/rtofs_glo_2ds_${hrs}_3hrly_diag.nc .
#  scp -p ${log}${CCSDIR}/rtofs_glo_2ds_${hrs}_3hrly_prog.nc .
#  scp -p ${log}${CCSDIR}/rtofs_glo_3dz_${hrs}_6hrly_reg1.nc .
#  scp -p ${log}${CCSDIR}/rtofs_glo_3dz_${hrs}_6hrly_reg2.nc .
#  scp -p ${log}${CCSDIR}/rtofs_glo_3dz_${hrs}_6hrly_reg3.nc .
done


#GFS
CCSDIR=/com/gfs/prod/gfs.$tag
for hr in f00 f120 f24 f384
do
  scp -p ${log}${CCSDIR}/gfs.t00z.pgrb2b$hr         .
  scp -p ${log}${CCSDIR}/gfs.t00z.sfluxgrb$hr.grib2 .
done

#CFS
#6hourly, monthly, time; 01 02 03 04
#  what is difference between 01, 02, 03, 04?
CCSDIR=/com/cfs/prod/cfs/cfs.$tag/00/6hrly_grib_0
for d in 1 2 3 4
do
  for fday in $tag $tag1 $tag5 $tag16 $tag30
  do
    scp -p ${log}${CCSDIR}$d/flxf${fday}00.0${d}.${tag}00.grb2 .
  done
done
#Note that the ocean does not have an f00 hour file.
for d in 1 2 3 4
do
  for fday in $tag1 $tag5 $tag16 $tag30
  do
    scp -p ${log}${CCSDIR}$d/ocnf${fday}00.0${d}.${tag}00.grb2 .
  done
done

#Rtofs-atl
#Skip -- no files have ice data
#CCSDIR=/com/ofs/prod/ofs.$tag
#for hrs in N000 F024 F120
#do
#  scp -p ${log}${CCSDIR}/ofs_atl.t00z.${hrs}.3d.grb .
#done
#for hrs in n000 f024 f120
#do
#  scp -p ${log}${CCSDIR}/ofs_atl.t00z.${hrs}.grb.grib2    .
#  scp -p ${log}${CCSDIR}/ofs_atl.t00z.${hrs}.atl.grb.std.grib2 .
#  scp -p ${log}${CCSDIR}/ofs_atl.t00z.${hrs}.atl.hires.grb.std.grib2 .
#done
