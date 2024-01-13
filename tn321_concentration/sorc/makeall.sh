#!/bin/sh
#Robert Grumbine
#22 December 2016

set -x

module list > /dev/null 2> /dev/null

if [ $? -ne 0 ] ; then
#On a system without the module software
  source ../versions/build.ver
  if [ `uname` == 'Darwin' ] ; then
    export MMAB_BASE=/Users/rmg3/usrlocal/mmablib/
    export MMAB_VER=""
  else
    export MMAB_BASE=/usr1/rmg3
    export MMAB_VER=""
  fi
  export VER=$MMAB_VER
  export MMAB_INC=$MMAB_BASE/$VER/include/
  export MMAB_LIB="-L ${MMAB_BASE}/$VER/"
  export MMAB_SRC=${MMAB_BASE}/${VER}/sorc/
  #end building on non-module system
else
#on a system with module software, such as wcoss
  source ../versions/build.ver
  set +x
  #module reset
  echo zzz pwd = `pwd`
  module use `pwd`/modulefiles
  module load seaice_amsr2/$seaice_amsr2_ver
  if [ $? -ne 0 ] ; then
    echo some problem trying to load seaice_amsr2/$seaice_amsr2_ver
    exit 1
  fi
  set -x
  module list
  env


  #export MMAB_BASE=`pwd`/mmablib/${MMAB_VER}
  export MMAB_BASE=$HOME/rgdev/mmablib/${MMAB_VER}
  export MMAB_INC=$MMAB_BASE/include
  export MMAB_SRC=$MMAB_BASE/sorc
  export MMAB_LIBF4=$MMAB_BASE/libombf_4.a

fi
export mmablib_ver=${MMAB_VER}
echo zzz mmab_inc $MMAB_INC

#set -xe
#set -x

for d in seaice_amsrbufr.fd seaice_iceamsr2.Cd 
do
  cp makeall.mk $d
  cd $d
  ./makeall.sh
  cd ..
done

if [ ! -d ../exec ] ; then
  mkdir ../exec
fi
./toexec cp

#clean up
rm */makeall.mk
