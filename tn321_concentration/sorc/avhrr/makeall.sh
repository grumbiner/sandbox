#!/bin/sh
#Robert Grumbine
#22 December 2016

set -x

source ../versions/build.ver
echo zzz seaice_analysis_ver = $seaice_analysis_ver

module list > /dev/null 2> /dev/null

if [ $? -ne 0 ] ; then
#On a system without the module software
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
  #set +x
  #module reset
  echo zzz pwd = `pwd`
  echo zzz seaice_analysis_ver = $seaice_analysis_ver

  module use `pwd`/modulefiles
  module load seaice_analysis/$seaice_analysis_ver
  if [ $? -ne 0 ] ; then
    echo some problem trying to load seaice_analysis/$seaice_analysis_ver
    exit 1
  fi
  set -x
  module list
  env

  export MMAB_BASE=~/rgdev/mmablib/${MMAB_VER}
  #export MMAB_BASE=`pwd`/mmablib/${MMAB_VER}
  export MMAB_INC=$MMAB_BASE/include
  export MMAB_SRC=$MMAB_BASE/sorc
  export MMAB_LIBF4=$MMAB_BASE/libombf_4.a

fi
export mmablib_ver=${MMAB_VER}

#------------------------------------------------------------
if [ -z $MMAB_BASE ] ; then
  echo MMAB_BASE for mmablib has not been defined
  exit
fi

if [ ! -f makeall.mk ] ; then
  cp ../makeall.mk .
  if [ $? -ne 0 ] ; then
    echo could not find makeall.mk, aborting
    exit 1
  fi
fi

if [ -z $MMAB_LIBF4 ] ; then
  export MMAB_LIBF4=$MMAB_BASE/libombf_4.a
fi


for d in seaice_avhrrfilter.Cd seaice_avhrrbufr.fd
do
  cd $d
  make
  cd ..
done
