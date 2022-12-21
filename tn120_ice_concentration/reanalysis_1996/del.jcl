yy=91
tag=${yy}1203
export tag yy
ODIR=/data/forecasts/ice.model/reanl
#set -x

tagm=`expr $tag - 1 `
tagm=`dtgfix3 $tagm`

while [ $tag -le 941231 ] 
#while [ $tag -le 911211 ] 
do
  
  yy=`echo $tag | cut -c1-2`
  mm=`echo $tag | cut -c3-4`
  dy=`echo $tag | cut -c5-6`
  base=${yy}${mm}
  tagm=`expr $tag - 1 `
  tagm=`dtgfix3 $tagm`

  echo $tag 

  grep ${tag} index.19$yy | wgrib ice.19$yy -i -nh -o ice.${tag}
  tmp=${tag}
  while [ ! -s ice.${tag} ] 
  do
    tmp=`expr ${tmp} - 1`
    tmp=`dtgfix3 $tmp` 
    echo Missing ice on ${base}${dy}. Trying $tmp
    grep ${tmp} index.19$yy | wgrib ice.19$yy -i -nh -o ice.${tag}
  done
      
  ./delta ice.$tag ../reanl/fill.$tag flag.$tag > delout.$tag

  rm ice.$tag
#  gzip delout.$tag
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`

done
