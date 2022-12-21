tag=19810901
while [ $tag -le 20110930 ]
do
  if [ ! -f qdoi.$tag ] ; then
    echo missing $tag
  fi
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
