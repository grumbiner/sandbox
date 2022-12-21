#!/bin/sh

#Main script for running the KISS sea ice model(s)
#Robert Grumbine
#Started Jan 2013
#Last modification 24 March 2014


cd $DATA

########################################
#set -x
msg="HAS BEGUN!"
postmsg "$jlogfile" "$msg"
###########################
PATH=$PATH:.

#------------------------------------------------------------------------
# construct the inputs
#------------------------------------------------------------------------
#A: Meteorology (from GFS/GDAS

echo Make reduced grib parameter files from gdas:
for gcyc in 00 06 12 18 
do
  for hh in 00 01 02 03 04 05
  do
    ln -sf ${COMgdas}/gdas1.t${gcyc}z.sfluxgrbf${hh} gdasin
    wgrib gdasin > index
    for parm in UFLX VFLX ICEC ICETK PRATE CPRAT  \
      TMP:kpds5=11:kpds6=105:kpds7=2       \
      UGRD:kpds5=33:kpds6=105:kpds7=10     \
      VGRD:kpds5=34:kpds6=105:kpds7=10     \
      SPFH:kpds5=51:kpds6=105:kpds7=2      \
      PRES:kpds5=1:kpds6=1:kpds7=0         \
      GFLUX:kpds5=155:kpds6=1:kpds7=0:TR=3 \
      SHTFL:kpds5=122:kpds6=1:kpds7=0:TR=3 \
      LHTFL:kpds5=121:kpds6=1:kpds7=0:TR=3 \
      DLWRF:kpds5=205:kpds6=1:kpds7=0:TR=3 \
      DSWRF:kpds5=204:kpds6=1:kpds7=0:TR=3 \
      USWRF:kpds5=211:kpds6=1:kpds7=0:TR=3 \
      TCDC:kpds5=71:kpds6=200
    do
      grep $parm index | wgrib -i gdasin -append -grib -o hindcastin.$gcyc
    done
  done
done


echo make reduced grib file for forecast inputs:

for hh in 06 12 18 24 30 36 42 48 54 60 66 72 78 84 90 96 102 108 114 120 126 132 138 144 150 156 162 168 174 180 186 192
do
  ln -sf ${COMgfs}/gfs.t00z.sfluxgrbf${hh} gfsin

  wgrib gfsin > index
  for parm in UFLX VFLX ICEC ICETK PRATE CPRAT\
   TMP:kpds5=11:kpds6=105:kpds7=2       \
   UGRD:kpds5=33:kpds6=105:kpds7=10     \
   VGRD:kpds5=34:kpds6=105:kpds7=10     \
   SPFH:kpds5=51:kpds6=105:kpds7=2      \
   PRES:kpds5=1:kpds6=1:kpds7=0         \
   GFLUX:kpds5=155:kpds6=1:kpds7=0:TR=3 \
   SHTFL:kpds5=122:kpds6=1:kpds7=0:TR=3 \
   LHTFL:kpds5=121:kpds6=1:kpds7=0:TR=3 \
   DLWRF:kpds5=205:kpds6=1:kpds7=0:TR=3 \
   DSWRF:kpds5=204:kpds6=1:kpds7=0:TR=3 \
   USWRF:kpds5=211:kpds6=1:kpds7=0:TR=3 \
   TCDC:kpds5=71:kpds6=200
  do
    grep $parm index | wgrib -i gfsin -append -grib -o forecastin
  done
done

