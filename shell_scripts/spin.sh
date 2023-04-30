tag=20050126
while [ $tag -le 20051231 ]
do
  if [ -f u.00 -o -f u.06 ] ; then
    rm u.00 u.06
  fi

  base=gdas/gdas.$tag
  wgrib $base/u.00.06.$tag | wgrib -i $base/u.00.06.$tag -nh -o u.00
  #wgrib $base/u.06.00.$tag | wgrib -i $base/u.06.00.$tag -nh -o u.06
  wgrib $base/u.00.00.$tag | wgrib -i $base/u.00.00.$tag -nh -o u.06
  
  if [ -f u.00 -a -f u.06 ] ; then
    echo tag  $tag `./spin u.00 u.06`
  fi

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
  
done


