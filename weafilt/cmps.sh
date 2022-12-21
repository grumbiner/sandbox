for sat in 246 248
do
  tag=20090101
  while [ $tag -le 20091231 ]
  do
    for pole in north south
    do
      if [ -f umask${pole}$sat.$tag ] ; then
        cmp -l umask${pole}$sat.$tag ../old/umask${pole}$sat.$tag > umask${pole}$sat.$tag.out
        x=`wc -l umask${pole}$sat.$tag.out`
        echo $x umask${pole}$sat.$tag
      fi
    done
    tag=`expr $tag + 1`
    tag=`dtgfix3 $tag`
  done
done

