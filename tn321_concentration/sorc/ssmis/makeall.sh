#!/bin/sh

if [ ! -f makeall.mk ] ; then
  cp ../makeall.mk .
  if [ $? -ne 0 ] ; then
    echo could not find makeall.mk, aborting
    exit 1
  fi
fi


for d in seaice_seaissmisu.cd seaice_ssmisubufr.fd
do
  cd $d
  make
  cd ..
done
