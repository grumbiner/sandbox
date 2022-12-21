#!/bin/sh
#export mmablib_ver=${MMAB_VER:-v3.5.0}

if [ ! -f makeall.mk ] ; then
  cp ../makeall.mk .
  if [ $? -ne 0 ] ; then
    echo could not find makeall.mk, aborting
    exit 1
  fi
fi


for d in seaice_avhrrfilter.Cd seaice_avhrrbufr.fd
do
  cd $d
  make
  cd ..
done
