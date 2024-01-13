yy=1978
while [ $yy -le 2014 ]
do
  if [ -d $yy ] ; then
    time tar czf ../shtbs.${yy}.tgz $yy
  fi
  yy=`expr $yy + 1`
done
