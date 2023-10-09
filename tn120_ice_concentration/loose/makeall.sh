#!/bin/sh
#Robert Grumbine
#22 December 2016

module
if [ $? -ne 0 ] ; then
#On a system without the module software
  export BASE=/usr1/
  export MMAB_VER=v3.4.3
else
#on a system with module software, such as wcoss
  module purge
  module load ./seaice_analysis.modulefile 
  module list
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
