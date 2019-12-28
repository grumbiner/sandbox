#!/bin/bash
#Script to read in the layers of the otis analysis and interpolate them
#  on to the cfs grid.  This is run automatically by the cron job which
#  archives the otis fields in the first place.  This script can
#  only be run by me (rmg3) but is included here for informational 
#  purposes.
#Robert Grumbine 15 August 1997

tag=`date +"%y%m%d"`
echo $tag
#tag=$1
base=`echo $tag | cut -c1-4`
dy=`echo $tag | cut -c5-6`

#If the qc'd file already exists, exit.
if [ -f /data/new/otis.cofs/long.$tag ] ; then
  exit
fi

if [ ! -d /tmp/otis ] ; then
  mkdir -p /tmp/otis
fi
cd /tmp/otis
rm *

cp ~rmg3/cpp/hotis .
cp ~rmg3/cpp/votis ./votis
cp ~rmg3/cfs.data/* .
gunzip *gz


case $tag in
  970815 | 970819 | 970823 | 970824 | 970825 )
    cp /data/new/otis.cofs/otis.$tag.grib .
    ;;
  971122 | 971123 | 971124 )
    cp /data/new/otis.cofs/otis.$tag.grib .
    ;;
  *)
    tar xvf /export/s2/otis/$tag.tar > /dev/null
    rm TEMP*
    cat ???X*GRB > otis.${tag}.grib
    ;;
esac

wgrib otis.${tag}.grib > index
#set -x

for level in 1 3 8 13 18 25 33 40 50 63 75 100 125 150 200 300 400 500 600 \
             700 800 900 1000 1100 1500 1750 2000 2500 3000 4000 5000
do
  x=`grep "kpds7=${level}:" index | grep TMP | wc -l`
  y=`grep "kpds7=${level}:" index | grep SALTY | wc -l`
  
    if [ $x -eq 1 ] ; then
      grep "kpds7=${level}:" index | grep TMP | wgrib otis.${tag}.grib -i -o tmp.$level -nh > /dev/null
      ./hotis tmp.$level tmp $level
     else
      echo Should have found 1 field for level $level Temperatures, found $x
      echo cannot retrieve this for later use.  Create a dummy level.
    fi
  
    if [ $y -eq 1 ] ; then
      grep "kpds7=${level}:" index | grep SALTY | wgrib otis.${tag}.grib -i -o salt.$level -nh > /dev/null
      ./hotis salt.$level salt $level
     else
      echo Should have found 1 field for level $level Salinities, found $y
      echo cannot retrieve this for later use.  Create a dummy level.
    fi
  
done
# At this point have all of the layers converted.  Now it is time to
#   assemble these in to the cofs vertical.
./votis asdf
cp tsout tsout.$tag 
cp tsout /data/new/otis.cofs/tsout.$tag

#Script to QC the otis fields generated daily and converted to the COFS grid.
#Version to start from July 1 and run up to the present.  The daily version
#  is a separate script, which will not need changing unless you add or delete
#  arguments to the qc program, which I recommend agains.
#Robert Grumbine 15 August 1997

#First I compute the July average, as noted.  
#Then I ran through July, initializing both the short term and long term averages
#  to the July average, using the July average as the QC field for the entire
#  period.
#Then, starting with August 1, performed the QC with the previous day's long
#  term average as the QC field.
#The fields are stored in /data/new/otis.cofs/
#-- with names qc.yymmdd, long.yymmdd, short.yymmdd
#To rerun to the present, add days to the line following base=9708
#  up to the present day of the month.  If you get in to September,
#  add a new loop and repeat the structure that August has.

#To change which qc program is used, make the cp ~/cofs/qcavg .
#  command refer to your own qcavg program.

cp ~rmg3/cpp/qcavg .

dm1=`expr ${base}$dy - 1`
dm1=`dtgfix3 $dm1`
cp /data/new/otis.cofs/long.$dm1 .
cp /data/new/otis.cofs/short.$dm1 .
cp long.$dm1 qc.$dm1
loadwait ./qcavg qc.$dm1 long.$dm1 short.$dm1 tsout.${base}$dy \
                long.${base}$dy short.${base}$dy 
cp long.${base}$dy short.${base}$dy /data/new/otis.cofs/
rcp *.${base}${dy}.asc cray4:/eldisk1/wd21rg/for.cofs
rm *.${base}${dy}.asc 

