#!/bin/sh


#General:
export PYTHONPATH=/u/Robert.Grumbine/para/mmablib/mmablib/py/
export SIDFEXdir=/u/Robert.Grumbine/para/mmablib/ice_scoring/sidfex

module load python/3.6.3

#./prep.sh -- replaced by targets.py

#need to call the getiabp with python2 -- urllib issues
python2 ./get.py


if [ ! -d out ] ; then
  mkdir out
fi
if [ ! -f forecast.points ] ; then
  echo no forecast.points file 
  exit 2
fi

if [ -f SIDFEx_targettable.txt ] ; then
  rm SIDFEx_targettable.txt
fi
wget https://swift.dkrz.de/v1/dkrz_0262ea1f00e34439850f3f1d71817205/SIDFEx_index/SIDFEx_targettable.txt

#For wcoss
time python targets.py
echo starting the drifter
time python $SIDFEXdir/sidfex.py
echo done with the drifter

cd out
if [ ! -f uploaded ] ; then
  find . -type f -exec $SIDFEXdir/upload.sh {} \;
else
  find . -type f -newer uploaded -name 'nc*' -exec $SIDFEXdir/upload.sh {} \;
fi
touch uploaded
