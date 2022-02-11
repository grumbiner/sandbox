#!/bin/sh

#Making a 'unit test' on the polar stereographic gribbing
#Run twice -- prod versions, and test versions
#Robert Grumbine 25 Jul 2011

#set -xe

tag4=20110725
unit_src=/u/wx21rg/rgdev/ice_unit

#Construct 25.4 km versions:  NEW:
for type in prod test
do
  FIX=/nwprod/fix/
  EXDIR=/nw${type}/exec/

  $EXDIR/seaice_north_reduce ${unit_src}/seaice.t00z.umasknorth12 \
                             ${type}_seaice.t00z.umasknorth \
                      $FIX/seaice_nland127.map $FIX/seaice_nland.map
  $EXDIR/seaice_south_reduce ${unit_src}/seaice.t00z.umasksouth12 \
                             ${type}_seaice.t00z.umasksouth \
                      $FIX/seaice_sland127.map $FIX/seaice_sland.map
done

#Now see if the output is the same:
echo Comparing outputs from reduce:
for pole in north south 
do
  cmp -l prod_seaice.t00z.umask${pole} test_seaice.t00z.umask${pole}
done
