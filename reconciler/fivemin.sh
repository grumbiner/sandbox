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
  #time ./refill $SHOREDIR/gshhs_c.b fout a.xpm > out1 2> out2 
  #time ./refill $SHOREDIR/try15.b fout a.xpm > out1 2> out2 
  #time ./refill $SHOREDIR/try8.b fout a.xpm > out1 2> out2 
  #time ./refill $SHOREDIR/try4.b fout a.xpm > out1 2> out2 
  #time ./refill $SHOREDIR/try1.b fout a.xpm > out1 2> out2 
  #time ./refill $SHOREDIR/gshhs_l.b fout a.xpm > out1 2> out2 
  time ./refill $SHOREDIR/gshhs_i.b fout a.xpm > out1 2> out2 
  #time nice -19 ./refill $SHOREDIR/gshhs_f.b fout_f aa_f.xpm > out1 2> out2 
#fi

if [ $? -eq 0  -a -f fout -a ! -f bathyout ] ; then
  time ./bathy fout $TOPODIR/etopo2.raw bathyout bath.xpm 0 0 > bathy.out
fi
