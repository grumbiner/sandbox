#!/bin/sh
#Robert Grumbine 12 Oct 2004

for fn in reconciler mmab.include shorelines etopo2 ncep_third demo reconciler.only
do
  cd $fn
  tar xvf ../${fn}.tar
  cd ..
done
