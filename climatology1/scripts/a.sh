yy=1980
while [ $yy -le 2014 ] 
do
  if [ -f ice.${yy}0101 ] ; then
    ./shannon ice.${yy}* > $yy
    mv shannon.out shannon.$yy
  fi
  yy=`expr $yy + 1`
done
