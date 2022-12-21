#!/bin/ksh

#Basic script for building the reconciled coastline/bathymetry files
#Robert Grumbine 19 August 2003

#You will need to reset the following environmental variables for your
#  system: 
SHOREDIR=shorelines
TOPODIR=etopo2
OUTDIR=expt

#Should need no changes below here -------------------------
make -f make.fivemin

# Now carry out procedures
#if [ ! -f fout ] ; then
  #time nice -19 ./refill $SHOREDIR/gshhs_f.b fout_f aa_f.xpm > out1 2> out2 
#fi

if [ $? -eq 0  -a -f fout_f -a ! -f bathyout ] ; then
  time ./bathy fout_f $TOPODIR/etopo2.raw bathyout bath.xpm 0 0 > bathy.out
fi
