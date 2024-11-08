yy=91
tag=${yy}1203
export tag yy
ODIR=/data/forecasts/ice.model/reanl
#set -x
./ones 

tagm=`expr $tag - 1 `
tagm=`dtgfix3 $tagm`
cp age1 $ODIR/latlon.$tagm
cp age1 $ODIR/fill.$tagm
cp age1 $ODIR/age.$tagm

#while [ $tag -le 950831 ] 
while [ $tag -le 911211 ]
#while [ $tag -le 921231 ]
do
  
  yy=`echo $tag | cut -c1-2`
  mm=`echo $tag | cut -c3-4`
  dy=`echo $tag | cut -c5-6`
  base=${yy}${mm}
  tagm=`expr $tag - 1 `
  tagm=`dtgfix3 $tagm`

  echo $tag 

  grep ${tag} ../sst/index.19$yy | wgrib ../sst/sst.19$yy -i -nh -o sst.${tag}
  tmp=${tag}
  while [ ! -s sst.${tag} ] 
  do
    tmp=`expr ${tmp} - 1`
    tmp=`dtgfix3 $tmp` 
    echo Missing sst on ${base}${dy}. Trying $tmp
    grep ${tmp} ../sst/index.19$yy | wgrib ../sst/sst.19$yy -i -nh -o sst.${tag}
  done
      
  tmp=$tag
  while [ ! -s ${base}.n/conc.$tmp ] 
  do
    tmp=`expr ${tmp} - 1`
    tmp=`dtgfix3 $tmp` 
    echo Missing n.ice on ${base}${dy}. Trying $tmp
    if [ -s ${base}.n/conc.$tmp ] ; then
      cp ${base}.n/conc.$tmp ${base}.n/conc.$tag
    fi
  done    
  tmp=$tag
  while [ ! -s ${base}.s/conc.$tmp ] 
  do
    tmp=`expr ${tmp} - 1`
    tmp=`dtgfix3 $tmp` 
    echo Missing s.ice on ${base}${dy}. Trying $tmp
    if [ -s ${base}.s/conc.$tmp ] ; then
      cp ${base}.s/conc.$tmp ${base}.s/conc.$tag
    fi
  done    

  ./filtnsidc sst.${tag} /export/s2/nsidc/bg/${base}.n/conc.${tag} /export/s2/nsidc/bg/${base}.s/conc.${tag} \
     $ODIR/latlon.$tag nps.$tag sps.$tag 275.0
  rm sst.$tag nps.$tag sps.$tag

  echo tfill
  ./tfill $ODIR/fill.$tagm $ODIR/latlon.$tag $ODIR/age.$tagm $ODIR/age.$tag $ODIR/fill.$tag 

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`

done
