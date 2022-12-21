tagm=20130625
tag=20130626
while [ $tag -le 20130701 ]
do
  export PDY=$tag
  export PDYm1=$tagm
  if [ -d /com/ofs/prod/ofs.$tag ] ; then
    time ./sms.fake > sms.$tag
  fi
  tagm=$tag
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
