#!/bin/ksh

#Basic script for building the reconciled coastline/bathymetry files
#Robert Grumbine 19 August 2003

#You will need to reset the following environmental variables for your
#  system: 
SHOREDIR=shorelines
TOPODIR=etopo2
OUTDIR=expt

#Should need no changes below here -------------------------
for TARG in great_lakes_10km 
#great_lakes_1km
do
  export TARG
  rm refill2
  make -f make.polar
  
  # Now carry out procedures
  #_c = 25 km, _l = 5 km, _i = 1 km, _f = 0.2 km
  #time ./refill $SHOREDIR/gshhs_c.b fout a.xpm > out1 2> out2 
  #time ./refill $SHOREDIR/gshhs_l.b fout a.xpm > out1 2> out2 
  #time ./refill $SHOREDIR/gshhs_i.b fout a.xpm > out1 2> out2 
  time ./refill2 $SHOREDIR/gshhs_f.b fout a2$TARG.xpm > out1.b 2> out2.b

done
