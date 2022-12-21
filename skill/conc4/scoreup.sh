tag=20140420
while [ $tag -le 20140430 ]
do
  export tag
  time ./scoreall.sh

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
