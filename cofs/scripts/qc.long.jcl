#!/bin/bash

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

cd /data/new/otis.cofs/
cp ~/cpp/qcavg .
cp july.avg long.970630 
cp july.avg short.970630 

base=9707
for dy in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 \
          21 22 23 24 25 26 27 28 29 30 31
do
  dm1=`expr ${base}$dy - 1`
  dm1=`dtgfix3 $dm1`
  ./qcavg july.avg long.$dm1 short.$dm1 tsout.${base}$dy long.${base}$dy \
                             short.${base}$dy 
done

base=9708
for dy in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 
do
  dm1=`expr ${base}$dy - 1`
  dm1=`dtgfix3 $dm1`
  cp long.$dm1 qc.$dm1
  ./qcavg qc.$dm1 long.$dm1 short.$dm1 tsout.${base}$dy \
                  long.${base}$dy short.${base}$dy 
done

