yy=95
tag=${yy}1001
j=0
export tag j yy

#wgrib sst.1995 > sst.index

while [ $tag -lt 951129 ] 
do
  
  mm=`echo $tag | cut -c3-4`
  dy=`echo $tag | cut -c5-6`
  base=${yy}${mm}
  tagm=`expr $tag - 1 `
  tagm=`dtgfix3 $tagm`

  echo $tag 

  dd conv=swab < n3ssmi.$base$dy > nrev
  dd conv=swab < s3ssmi.$base$dy > srev

  recomp nrev srev nconc.$base$dy sconc.$base$dy

  grep ${tag} sst.index | wgrib sst.19$yy -i -nh -o sst.${tag}
  tmp=${tag}
  while [ ! -s sst.${tag} ] 
  do
    tmp=`expr ${tmp} - 1`
    tmp=`dtgfix3 $tmp` 
    echo Missing sst on ${base}${dy}. Trying $tmp
    grep ${tmp} sst.index | wgrib sst.19$yy -i -nh -o sst.${tag}
  done
      
  filtbg sst.${tag} /export/s1/ssmi/nconc.${tag} /export/s1/ssmi/sconc.${tag} \
     latlon.$tag nps.$tag sps.$tag 275.0
  rm nps.$tag sps.$tag

  tfill fill.$tagm latlon.$tag age.$tagm age.$tag fill.$tag 

  j=`expr $j + 1`
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`

done
