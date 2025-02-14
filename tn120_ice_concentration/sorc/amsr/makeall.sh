#!/bin/sh

pwd=`pwd`;

if [ ! -f makeall.mk ] ; then
  cp ../makeall.mk .
  if [ $? -ne 0 ] ; then
    echo could not find makeall.mk, aborting
    exit 1
  fi
fi

for d in seaice_amsrbufr.fd seaice_amsrice.Cd
do
  cd $pwd/$d
  make
  cd ..
done
