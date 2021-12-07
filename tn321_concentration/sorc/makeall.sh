#!/bin/sh
#Robert Grumbine
#22 December 2016

set -xe

module
if [ $? -ne 0 ] ; then
#On a system without the module software
  export BASE=/usr1/rmg3/
  export MMAB_VER=v3.4.3
  export VER=$MMAB_VER
  export MMAB_INC=$BASE/mmab/$VER/include/
  export MMAB_LIB=-L $BASE/mmab/$VER/
  export MMAB_SRC=$BASE/mmab/$VER/sorc/
else
#on a system with module software, such as wcoss
  module purge
  module load ./seaice_analysis.modulefile 
  module list
#If being built against new mmablib by developer:
  export BASE=~/save/
  export MMAB_VER=v3.4.3
  export VER=$MMAB_VER
  export MMAB_INC=$BASE/mmab/$VER/include/
  export MMAB_SRC=$BASE/mmab/$VER/sorc/
  export MMAB_LIB='-L $BASE/mmab/$VER/'
#from nco modulefile in /nwprod2/lib/modulefiles/mmab
  export dlib=$BASE/mmab/$VER
  export MMAB_INC=$dlib/include
  export MMAB_OMBC_LIB4=$dlib/libombc_4.a
  export MMAB_OMBF_LIB4=$dlib/libombf_4.a

fi

. ../versions/seaice_analysis.ver

for d in general amsr2 ssmi ssmis avhrr 
do
  cp makeall.mk $d
  cd $d
  ./makeall.sh
  cd ..
done
./toexec
