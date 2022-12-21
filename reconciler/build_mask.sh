#!/bin/ksh

#Basic script for building the reconciled coastline/bathymetry files
#Robert Grumbine 19 August 2003
#Updates 21 April 2010

#You will need to reset the following environmental variables for your
#  system: 
GRIDDIR=~/usrlocal/data/
SHOREDIR=/usr2/rmg3/allbathy/shorelines
TOPODIR=/usr2/rmg3/allbathy/etopo2
OUTDIR=expt

export FAMILY=resops
export TARGET=hycom
export RESOPS=1

#Should need no changes below here -------------------------
make refill bathy toab paving

# Now carry out procedures
  ln -sf ${GRIDDIR}/fort.061a .
  ln -sf ${GRIDDIR}/fort.61 .

  if [ ! -f fout ] ; then
    time ./refill $SHOREDIR/gshhs_f.b fout a.xpm > out1 2> out2 
  fi
  
  if [ $? -eq 0  -a -f fout -a ! -f bathyout ] ; then
    ./bathy fout $TOPODIR/etopo2.raw bathyout bath.xpm 0 0 > hycom.bathy.out
  fi

  if [ -f bathyout ] ; then
    ./toab bathyout bathymask.a bathymask.b
    #Note that 40, 330 is the N and E coordinate of a point we consider ocean
    ./paving bathyout 40.0 330.0 paveout straits
  fi 

  if [ -f paveout ] ; then
    ./toab paveout paved.a paved.b
  fi

  if [ ! -d $OUTDIR ] ; then
    mkdir $OUTDIR
  fi
  mv *.xpm fout bathyout bathymask.a bathymask.b paveout paved.a paved.b $OUTDIR
  
