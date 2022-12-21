#!/bin/ksh

#Basic script for building the reconciled coastline/bathymetry files
#Robert Grumbine 19 August 2003
#Extended and generalized 9 July 2007

#You will need to reset the following environmental variables for your
#  system: 
SHOREDIR=/usr1/allbathy/shorelines
TOPODIR=/usr1/allbathy/etopo2

#This establishes the general family of grid (lat-long, resop, ...) and
#  then the specific target to be used.  Intermediate utility grids
#  of type 'FAMILY' are created by programs paving and refill as space
#  and time savers 

set -x
#resops = 1 for resops, 0 for psgrid, 2 for llgrid  Future: fix
FAMILY=${FAMILY:-llgrid}
TARGET=${TARGET:-global_quarter}
RESOPS=${RESOPS:-2}
LOW=${LOW:-global_ice}

export FAMILY TARGET RESOPS LOW
#export FAMILY=llgrid
#export TARGET=global_sst
#export RESOPS=2
if [ $TARGET = "hycom" ] ; then
  GRIDDIR=/usr/local/data/
  export RESOPS=1
fi

#Note that shorefile may be any of gshhs_c.b, gshhs_l.b, gshhs_i.b, gshhs_f.b
#  or files of your own construction from the GMT gshhs suite
#    _c = 25 km, _l = 5 km, _i = 1 km, _f = 0.2 km
#SHOREFILE=gshhs_i.b
#SHOREFILE=try0.2.b
SHOREFILE=${SHOREFILE:-gshhs_i.b}

#Program bathy is hardwired to etopo2 file.  Easily rewritten, but hardwired.
#Set the following to true if you want to pave over selected straits
#  then edit the straits file to include the straits you want.
export straits=false

OUTDIR=${FAMILY}/${TARGET}/$SHOREFILE


#Should need no changes below here -------------------------

# Now carry out procedures

  if [ ! -f fout ] ; then
    make telescope
    time ./telescope $SHOREDIR/$SHOREFILE fout a.xpm master.global_ice> out1 2> out2 
  fi
  


  #Distribute the output:
  if [ ! -d $OUTDIR ] ; then
    mkdir -p $OUTDIR
  fi
  mv *.xpm out1 out2 fout $OUTDIR
