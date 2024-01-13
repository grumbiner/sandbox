tag=20081202
while [ $tag -le 20090107 ]
do
  if [ -f compout.$tag ] ; then
    grep low compout.$tag | ./lowkml.pl 10 > low10.$tag.kml
    grep hi  compout.$tag | ./hikml.pl  15 > hi15.$tag.kml
    grep rtg compout.$tag | ./rtgkml.pl 20 > rtg20.$tag.kml
  fi

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
