#!/bin/sh

#BASE is location for where MMABLIB is
export BASE=${BASE:-/u/Robert.Grumbine/save/}

for d in rtofs_avg.Cd/ rtofs_gsf.Cd/ rtofs_trace.Cd/ 
do
  cd $d
  make
  cd ..
  execname=`echo $d | cut -f1 -d\.`
  mv $d/$execname .
done

