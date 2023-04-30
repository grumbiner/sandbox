#!/bin/sh

#cd /ptmp/wx21rg/
#cp /nfsuser/g01/wx21rg/test/meteo .

#Loop over last few months, get the flux files and construct the
#  ice grid files.
FDIR=~/sdb/fluxes/2003/

tag=20030101
while [ $tag -le 20030131 ]
do
  for hh in 00 06 12 18
  do
    if [ -f $FDIR/flxf06.${tag}$hh ] ; then
      wgrib $FDIR/flxf06.${tag}$hh | wgrib -i $FDIR/flxf06.${tag}$hh -nh -o metin
      time ./meteo
      mv metout.north north.${tag}$hh
      mv metout.south south.${tag}$hh
    else
      echo missing flux file ${tag}$hh
    fi
  done
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
