#!/bin/ksh
#Script to build the sea ice model boundary condition files:
#  tsdeep, tsshal, MASK

#BASE is the directory you unpacked icemodel.tar in
#FLDFLAGS are flags for the linker/loader
#FORT is everything other than unit number in the default fortran file names
#FFC is the fortran compiler

if [ -f /etc/linux ] ; then
   BASE=${BASE:-/usr1/rmg3/}
   FLDFLAGS=${FLDFLAGS:-}
   FORT=${FORT:-fort.} 
   FFC='f77 -O2'
   FLD='f77 '
   DHOME=${DHOME:-/usr1/rmg3/data/}
   FORO=${FORO:-/tmp}
   TDIR=${TDIR:-/tmp}
else
   BASE=${BASE:-/u/wx21rg/nfsuser/}
   FLDFLAGS=${FLDFLAGS:-}
   FORT=${FORT:-fort.} 
   FFC='xlf -O2'
   FLD='xlf '
   DHOME=${DHOME:-/nfsuser/g01/marine/climate}
   FORO=${FORO:-/nfstmp/wx21rg}
   TDIR=${TDIR:-/nfstmp/wx21rg}
fi
export BASE FLDFLAGS FORT FFC FLD


#Must Define RHOME, IHOME DHOME, pole, FORO, TDIR
#DHOME is location of bathy.bin, zmask
#FORO is location of files being made for others
#TDIR is directory for temporary execution -- uses large files

RHOME=${RHOME:-$BASE/icemodel}
MODDIR=${MODDIR:-$BASE/icemodel/}
IHOME=${IHOME:-$BASE/icemodel/mpi.source}
OHOME=${OHOME:-$BASE/lib/}
LIBDIR=${LIBDIR:-$BASE/lib/}
PHOME=${PHOME:-$BASE/lib/}
pole=${pole:-north}
export RHOME IHOME DHOME FORO TDIR pole OHOME LIBDIR PHOME 

#Should need no changes below here--------------

set -xe

if [ ! -d $TDIR ] ; then
  mkdir -p $TDIR
fi

cd ${RHOME}/bc
echo Entered the boundary condition builder

if [ -f $IHOME/icegrid.inc ] ; then
  chmod 644 ${IHOME}/icegrid.inc
fi
cp ${IHOME}/icegrid.$pole ${IHOME}/icegrid.inc
chmod 644 ${IHOME}/icegrid.inc

if [ -f icegrid.inc ] ; then
  make -i clean
fi

#-----------------------------------------------------------------
# Create the ocean t, s files
retdir=`pwd`
make -i ocean
cp maskout $TDIR/MASK
mv ocean $TDIR
cd $TDIR
if [ -f tsdeep ] ; then
  rm tsdeep tsshal SALTOUT TEMPOUT
fi
if [ -f SALTOUT ] ; then
  rm SALTOUT TEMPOUT
fi
if [ -f outliers ] ; then
  rm outliers
fi
#moved ocean from bc to temp dir
time ./ocean < $retdir/${pole}.oin > oceanout
cp MASK tsdeep tsshal ${RHOME}/running/$pole
mv MASK tsdeep tsshal oceanout outliers ${RHOME}/bc/$pole
if [ -f SALTOUT ] ; then
  rm SALTOUT TEMPOUT
fi
cd $retdir

chmod 644 ${FORO}/mask* ${FORO}/latlong* ${FORO}/list*
rm *.o *.inc
