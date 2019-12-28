#!/bin/bash

#Script to QC the otis fields generated daily and converted to the COFS grid.
#Version run on a daily basis -- this assumes that the appropriate files
#  for yesterday (and prior days) have been created, e.g. by the long
#  qc script qc.long.jcl.
#This script must be run after the tsout file for the day is generated.
#  tsout.yymmdd is generated when the files are archived in the first place.
#  If tsout.yymmdd does not exist, the program will give a message to that
#  effect and exit.  No harm will occur.
#Robert Grumbine 15 August 1997

#Again, be sure that qcavg refers to the version of the program that
#  you want to be using, which probably is not ~/cofs/qcavg.

cd /data/new/otis.cofs/
cp ~/cofs/qcavg .

tag=`date +"%y%m%d"`
dm1=`expr $tag - 1`
dm1=`dtgfix3 $dm1`
cp long.$dm1 qc.$dm1
./qcavg qc.$dm1 long.$dm1 short.$dm1 tsout.$tag \
                long.$tag short.$tag 
