FIX=ssmi/fix

for fn in old_debuf new_old
do
  echo $fn > delta
  for sat in 15
  do
    satno=`expr $sat + 233`
    time ssmi1/exec/seaissmi.x delta $FIX/nland.map $FIX/sland.map \
       n3ssmi.$fn s3ssmi.$fn umasknorth.$fn umasksouth.$fn \
       b3north.$fn b3south.$fn 270 2009 $satno > v.$fn
  done
done  
