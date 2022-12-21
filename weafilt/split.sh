for n in 2 3 
do
  for type in lt gt
  do
    grep "trial $n $type" alpha > p$n.$type
  done
done

n=5
while [ $n -le 29 ] 
do
  for type in lt gt
  do
    grep "trial $n $type" alpha > p$n.$type
  done
  n=`expr $n + 1`
done
