#!/bin/sh
#Robert Grumbine
#Oct  9  2014 

#runs on polar (desktop linux) to produce the day 8 skill scores

for model in kiss_v0.0.0 gfs_v1.0.0 cfs_v2.0.0
do
  grep 'day 08 ' ~/www/develop/icemodel/$model/scoreout*   > ${model}.8
done 
gnuplot ~/bin/gnuplot_in
mv *8.png ~/www/develop/icemodel/
