#Recompute ice concentrations for october and november, 
#prepare for inline generation of reanalysis ice concentrations
#Bob Grumbine

base=9510
j=0
while [ $j -lt 31 ] 
do
  j=`expr $j + 1`
  if [ $j -lt 10 ] ; then
    dy=0$j
  else
    dy=$j
  fi
  dd conv=swab < n3ssmi.$base$dy > nrev
  dd conv=swab < s3ssmi.$base$dy > srev

  recomp nrev srev nconc.$base$dy sconc.$base$dy

done

base=9511
j=0
while [ $j -lt 28 ] 
do
  j=`expr $j + 1`
  if [ $j -lt 10 ] ; then
    dy=0$j
  else
    dy=$j
  fi
  dd conv=swab < /data/forecasts/ice.analy/n3ssmi.$base$dy > nrev
  dd conv=swab < /data/forecasts/ice.analy/s3ssmi.$base$dy > srev

  recomp nrev srev nconc.$base$dy sconc.$base$dy
done
