#!/bin/sh

export BASE=${BASE:-/u/Robert.Grumbine/save}

for d in seaice_amsrbufr.fd seaice_amsrice.cd
do
  cd $d
  make
  cd ..
done
