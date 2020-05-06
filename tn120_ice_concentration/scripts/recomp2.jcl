#!/bin/bash
#Recompute ice concentrations for october and november, 
#prepare for inline generation of reanalysis ice concentrations
#Bob Grumbine


for base in 9509 9510 9511 9512 9601 9602 9603 9604
do
  j=0
  while [ $j -lt 31 ] 
  do
    j=`expr $j + 1`
    if [ $j -lt 10 ] ; then
      dy=0$j
    else
      dy=$j
    fi
    dd conv=swab < $base/n3ssmi.$base$dy > nrev
    dd conv=swab < $base/s3ssmi.$base$dy > srev

    ./recomp nrev srev output/nconc.$base$dy output/sconc.$base$dy

  done
done

