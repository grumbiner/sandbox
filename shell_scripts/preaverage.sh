tag=20050125
tagp=20050126

while [ $tag -le 20051231 ]
do

base=gdas/gdas.$tag
base2=gdas/gdas.$tagp
for hh in 00 06 12 18
do
  if [ -f u.${hh}.00 ] ; then
    rm u.${hh}.00
  fi
  if [ -f u.${hh}.03 ] ; then
    rm u.${hh}.03
  fi
  if [ -f v.${hh}.00 ] ; then
    rm v.${hh}.00
  fi
  if [ -f v.${hh}.03 ] ; then
    rm v.${hh}.03
  fi
done


 
wgrib $base/u.00.00.$tag | wgrib -i $base/u.00.00.$tag -nh -o u.00.00
wgrib $base/u.00.03.$tag | wgrib -i $base/u.00.00.$tag -nh -o u.00.03
wgrib $base/u.06.00.$tag | wgrib -i $base/u.06.00.$tag -nh -o u.06.00
wgrib $base/u.06.03.$tag | wgrib -i $base/u.06.00.$tag -nh -o u.06.03
wgrib $base/u.12.00.$tag | wgrib -i $base/u.12.00.$tag -nh -o u.12.00
if [ -f  u.00.00 -a -f  u.00.03 -a -f  u.06.00 -a -f u.06.03 -a -f  u.12.00 ] ; then 
  ./preaverage gdas/uavg.${tag}00 u.00.00 u.00.03 u.06.00 u.06.03 u.12.00
fi

wgrib $base/v.00.00.$tag | wgrib -i $base/v.00.00.$tag -nh -o v.00.00
wgrib $base/v.00.03.$tag | wgrib -i $base/v.00.00.$tag -nh -o v.00.03
wgrib $base/v.06.00.$tag | wgrib -i $base/v.06.00.$tag -nh -o v.06.00
wgrib $base/v.06.03.$tag | wgrib -i $base/v.06.00.$tag -nh -o v.06.03
wgrib $base/v.12.00.$tag | wgrib -i $base/v.12.00.$tag -nh -o v.12.00
if [ -f v.00.00 -a -f v.00.03 -a -f v.06.00 -a -f v.06.03 -a -f v.12.00 ] ; then 
  ./preaverage gdas/vavg.${tag}00 v.00.00 v.00.03 v.06.00 v.06.03 v.12.00
fi
# -----------------------------
for hh in 00 06 12 18
do
  if [ -f u.${hh}.00 ] ; then
    rm u.${hh}.00
  fi
  if [ -f u.${hh}.03 ] ; then
    rm u.${hh}.03
  fi
  if [ -f v.${hh}.00 ] ; then
    rm v.${hh}.00
  fi
  if [ -f v.${hh}.03 ] ; then
    rm v.${hh}.03
  fi
done

wgrib $base/u.12.00.$tag | wgrib -i $base/u.00.00.$tag -nh -o u.12.00
wgrib $base/u.12.03.$tag | wgrib -i $base/u.00.00.$tag -nh -o u.12.03
wgrib $base/u.18.00.$tag | wgrib -i $base/u.06.00.$tag -nh -o u.18.00
wgrib $base/u.18.03.$tag | wgrib -i $base/u.06.00.$tag -nh -o u.18.03
wgrib $base2/u.00.00.$tagp | wgrib -i $base2/u.00.00.$tagp -nh -o u.00.00
if [ -f u.12.00 -a -f u.12.03 -a -f u.18.00 -a -f u.18.03 -a -f u.00.00 ] ; then
  ./preaverage gdas/uavg.${tag}12 u.12.00 u.12.03 u.18.00 u.18.03 u.00.00
fi

wgrib $base/v.12.00.$tag | wgrib -i $base/v.12.00.$tag -nh -o v.12.00
wgrib $base/v.12.03.$tag | wgrib -i $base/v.12.03.$tag -nh -o v.12.03
wgrib $base/v.18.00.$tag | wgrib -i $base/v.18.00.$tag -nh -o v.18.00
wgrib $base/v.18.03.$tag | wgrib -i $base/v.18.03.$tag -nh -o v.18.03
wgrib $base2/v.00.00.$tagp | wgrib -i $base2/v.00.00.$tagp -nh -o v.00.00
if [ -f v.12.00 -a -f v.12.03 -a -f v.18.00 -a -f v.18.03 -a -f v.00.00 ] ; then
  ./preaverage gdas/vavg.${tag}12 v.12.00 v.12.03 v.18.00 v.18.03 v.00.00
fi

#---------------
  tag=$tagp
  tagp=`expr $tag + 1`
  tagp=`dtgfix3 $tagp`

done
