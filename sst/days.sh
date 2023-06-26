tag=20081202
while [ $tag -le 20081217 ]
do

  for res in low hi
  do
    cat compout.$tag | grep $res | ./tokml.pl > ${res}15_$tag.kml
  done
 
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`

done
