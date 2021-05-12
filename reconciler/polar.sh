#!/bin/ksh

#Basic script for building the reconciled coastline/bathymetry files
#Robert Grumbine 19 August 2003

#You will need to reset the following environmental variables for your
#  system: 
SHOREDIR=shorelines
TOPODIR=etopo2
OUTDIR=expt

#Should need no changes below here -------------------------
for TARG in southhigh northgrid northhigh
do
  export TARG
  rm refill bathy
  make -f make.polar
  
  # Now carry out procedures
  #_c = 25 km, _l = 5 km, _i = 1 km, _f = 0.2 km
  #time ./refill $SHOREDIR/gshhs_c.b fout a.xpm > out1 2> out2 
  #time ./refill $SHOREDIR/gshhs_l.b fout a.xpm > out1 2> out2 
  time ./refill $SHOREDIR/gshhs_i.b fout a.xpm > out1 2> out2 
  #time ./refill $SHOREDIR/gshhs_f.b fout a.xpm > out1 2> out2 

  if [ $? -eq 0  -a -f fout -a ! -f bathyout ] ; then
    time ./bathy fout $TOPODIR/etopo2.raw bathyout bath.xpm 0 0 > bathy.out
  fi

  if [ ! -d $TARG ] ; then
    mkdir $TARG
  fi
  mv out1 out2 fout bathyout *.xpm bathy.out $TARG
done
