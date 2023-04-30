tag=920101
cut=60
while [ $tag -le 921231 ] 
do
  yy=`echo $tag | cut -c1-2`
  mo=`echo $tag | cut -c3-4`
  dd=`echo $tag | cut -c5-6`

  grep $tag index.19$yy | wgrib -i ice.19$yy -o ice.$tag
  ./area ice.$tag 0.$cut >> area$cut.$yy
  rm ice.$tag

  grep $tag index.19${yy}.new | wgrib -i ice.19${yy}.new -o ice.$tag
  ./area ice.$tag 0.$cut >> area$cut.${yy}.new
  rm ice.$tag

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`

done
