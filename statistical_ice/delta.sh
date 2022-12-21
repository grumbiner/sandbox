
tag=19951001
while [ $tag -le 20020930 ] 
do
  base=`echo $tag | cut -c5-8`

  ./delta avg.$base fill1.$tag delta.$tag

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`  
done
