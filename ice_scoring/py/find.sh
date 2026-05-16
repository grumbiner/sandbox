tag=20151101
while [ $tag -le 20180419 ]
do
  if [ ! -f edges/edge.$tag ] ; then
    if [ -f analy/analy.$tag ] ; then
      ln -sf analy/analy.$tag testin
      python findedge_main.py > edges/edge.$tag
    else
      echo no analysis file for $tag either
    fi
  fi

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
