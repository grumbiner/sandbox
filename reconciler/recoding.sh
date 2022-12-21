#!/bin/sh
#Robert Grumbine 29 April 2010
for gtype in global_12th global_ice northgrid southgrid northhigh southhigh
do
g++ -Wall -O2 recode.C -DGRIDTYPE=$gtype /usr/local/lib/libcppf -o recode_$gtype -DLINUX
done

for gtype in global_12th global_ice
do
  ./recode_$gtype llgrid/$gtype/gshhs_f.b/fout master.$gtype ice.$gtype sst.$gtype
done

for gtype in northgrid southgrid northhigh southhigh
do
  ./recode_$gtype psgrid/$gtype/gshhs_f.b/fout master.$gtype ice.$gtype sst.$gtype
done


