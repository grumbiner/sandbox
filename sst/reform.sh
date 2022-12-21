tag=19810901
while [ $tag -le 20110430 ]
do
  yy=`echo $tag | cut -c1-4`

  if [ -f ${yy}/avhrr-only-v2.$tag ] ; then
    dd conv=swab if=${yy}/avhrr-only-v2.$tag of=reformed/qdoi.$tag
  else
    echo missing $tag
  fi

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
