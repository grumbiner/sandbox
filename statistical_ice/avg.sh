
tag=20010101
while [ $tag -le 20011231 ] 
do
  base=`echo $tag | cut -c5-8`

  ./average avg.$base fill1.????$base

  ./nxpm avg.$base nland a$base.xpm

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`  
done
