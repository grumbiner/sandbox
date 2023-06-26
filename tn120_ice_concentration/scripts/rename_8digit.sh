tag=980701
while [ $tag -lt 981118 ] 
do
  mv n$tag.gif nh.19$tag.gif
  mv s$tag.gif sh.19$tag.gif
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
