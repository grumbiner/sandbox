#!/bin/sh

#Making a 'unit test' on the polar stereographic gribbing
#Run twice -- prod versions, and test versions
#Robert Grumbine 25 Jul 2011

#set -xe

tag4=20110725
unit_src=/u/wx21rg/rgdev/ice_unit

#Test the construction of xpm output files:
OUTDIR=/u/wx21rg/rgdev/ice_unit
  cp $OUTDIR/seaice.t00z.umasknorth12    umasknorth12
  cp $OUTDIR/seaice.t00z.umasksouth12    umasksouth12
  cp $OUTDIR/test_seaice.t00z.umasknorth umasknorth
  cp $OUTDIR/test_seaice.t00z.umasksouth umasksouth

#Construct 25.4 km versions:  NEW:
for type in prod test
do
  FIX=/nwprod/fix/
  EXDIR=/nw${type}/exec/

   $EXDIR/seaice_north12xpm umasknorth12 $FIX/seaice_nland127.map ${type}_nh12.xpm
   convert ${type}_nh12.xpm ${type}_nh12.gif

   $EXDIR/seaice_south12xpm umasksouth12 $FIX/seaice_sland127.map ${type}_sh12.xpm
   convert ${type}_sh12.xpm ${type}_sh12.gif

   $EXDIR/seaice_northxpm umasknorth $FIX/seaice_nland.map ${type}_nh.xpm
   convert ${type}_nh.xpm ${type}_nh.gif

   $EXDIR/seaice_southxpm umasksouth $FIX/seaice_sland.map ${type}_sh.xpm
   convert ${type}_sh.xpm ${type}_sh.gif

   rm ${type}_*.xpm 

done


#Now see if the output is the same:
echo Comparing outputs from reduce:
for pole in nh sh nh12 sh12
do
  cmp -l prod_${pole}.gif test_${pole}.gif
done
