#!/bin/ksh

#Script to read the result of a call to wgrib -V applied to a hurricane
#model output grib file and echo out the grid parameters needed by
#the hurricane -> regional wave model interpolation program

#Robert Grumbine
#12 August 1998

#set -x

read line1
read line2
read a b c d e f g g2 g3 NX h NY i
#echo $a $b $c $d $e $f $g $g2 $g3 $NX $h $NY $i
read line4
read a b LAT1 c d e DLAT j
read a   LON1 c d e DLON j
echo $NX $NY $LAT1 $DLAT $LON1 $DLON
