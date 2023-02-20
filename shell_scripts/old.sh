n=39
nm=44
tot=0
while [ $n -ge 30 ]
do
  x=`year $n $nm | wc -l`
  tot=`expr $tot + $x`
  if [ $x -gt 0 ] ; then
    echo $x files of $tot year $n
  fi
  nm=$n
  n=`expr $n - 1`
done
