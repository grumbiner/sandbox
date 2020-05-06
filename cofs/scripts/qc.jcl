#!/bin/bash
#Script to read in the layers of the otis analysis and interpolate them
#  on to the cfs grid.  This is run automatically by the cron job which
#  archives the otis fields in the first place.  This script can
#  only be run by me (rmg3) but is included here for informational 
#  purposes.
#Robert Grumbine 15 August 1997

if [ ! -d /tmp/otis ] ; then
  mkdir -p /tmp/otis
fi
cd /data/new/otis.cofs

cp ~rmg3/cfs.data/* .
cp ~rmg3/cpp/hotis .
cp ~rmg3/cpp/votis ./votis
gunzip *gz

tag=$1
base=`echo $tag | cut -c1-4`
dy=`echo $tag | cut -c5-6`

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
#cp /data/new/otis.cofs/long.$dm1 .
#cp /data/new/otis.cofs/short.$dm1 .
cp long.$dm1 qc.$dm1
loadwait ./qcavg qc.$dm1 long.$dm1 short.$dm1 tsout.${base}$dy \
                long.${base}$dy short.${base}$dy 
#cp long.${base}$dy short.${base}$dy /data/new/otis.cofs/
rcp *.${base}${dy}.asc cray4:/eldisk1/wd21rg/for.cofs
rm *.${base}${dy}.asc
