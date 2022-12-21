#!/bin/sh
#Robert Grumbine 29 April 2010

mv 5min_histogram histo_global_12th
mv 30min_histogram histo_global_ice
mv north_high histo_northhigh
mv north_low histo_northhgrid
mv south_low histo_southgrid
mv south_high histo_southhigh

OPS=/usr/local/data

g++ -Wall -O2 -DLINUX -DGRIDTYPE=global_12th compare.C /usr/local/lib/libcppf -o compare_global_12th
./compare_global_12th ice.global_12th $OPS/seaice_gland5min > global_12th.out

g++ -Wall -O2 -DLINUX -DGRIDTYPE=global_ice compare.C /usr/local/lib/libcppf -o compare_global_ice
./compare_global_ice  ice.global_ice  $OPS/seaice_newland > global_ice.out


g++ -Wall -O2 -DLINUX -DGRIDTYPE=northhigh compare.C /usr/local/lib/libcppf -o compare_northhigh
./compare_northhigh   ice.northhigh   $OPS/seaice_nland127.map > northhigh.out

g++ -Wall -O2 -DLINUX -DGRIDTYPE=northgrid compare.C /usr/local/lib/libcppf -o compare_northgrid
./compare_northgrid   ice.northgrid   $OPS/seaice_nland.map > northgrid.out

g++ -Wall -O2 -DLINUX -DGRIDTYPE=southhigh compare.C /usr/local/lib/libcppf -o compare_southhigh
./compare_southhigh   ice.southhigh   $OPS/seaice_sland127.map > southhigh.out

g++ -Wall -O2 -DLINUX -DGRIDTYPE=southgrid compare.C /usr/local/lib/libcppf -o compare_southgrid
./compare_southgrid   ice.southgrid   $OPS/seaice_sland.map > southgrid.out


exit

g++ -Wall -O2 -DLINUX -DGRIDTYPE=global_12th sstcompare.C /usr/local/lib/libcppf -o sst_hires
./sst_hires sst.global_12th sst.dat.old

g++ -Wall -O2 -DLINUX -DGRIDTYPE=global_ice sstcompare.C /usr/local/lib/libcppf -o sst_lores
./sst_lores sst.global_ice  sst.lores.old
