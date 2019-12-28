cd /export/s2/reanl

set -x

tag=960314
tagm=`expr $tag - 1 `
tagm=`dtgfix3 $tagm`

while [ $tag -le 960317 ] 
do

  yy=`echo $tag | cut -c1-2`

#recompute ice concentrations - put in umask
  stag=$tagm
  while [ ! -s /data/forecasts/ice.analy/n3ssmi.$tag ] 
  do
    if [ -s /data/forecasts/ice.analy/n3ssmi.$stag ] ; then
      cp /data/forecasts/ice.analy/n3ssmi.$stag /data/forecasts/ice.analy/n3ssmi.$tag
      cp /data/forecasts/ice.analy/s3ssmi.$stag /data/forecasts/ice.analy/s3ssmi.$tag
    else
      stag=`expr $stag - 1 `
      stag=`dtgfix3 $stag`
      if [ $stag -lt 951208 ] ; then
        echo ran ice back before archive!
        exit -1
      fi
    fi
  done

    
  dd conv=swab if=/data/forecasts/ice.analy/n3ssmi.$tag of=n3ssmi.$tag
  dd conv=swab if=/data/forecasts/ice.analy/s3ssmi.$tag of=s3ssmi.$tag
  ./recomp n3ssmi.$tag s3ssmi.$tag umasknorth.$tag umasksouth.$tag
  if [ ! -s umasknorth.$tag -a ! -s umasksouth.$tag ] ; then
    echo failed to produce nonzero sized umask files for $tag
    exit -1
  fi

  if [ -s umasknorth.$tag ] ; then
    mv umask*.$tag ice
#    rm ice/?3ssmi.$tag ?3ssmi.$tag ?ps.$tag
    rm ?3ssmi.$tag ?ps.$tag
   else
    echo failed to generate latlon file! $tag
    exit -1
  fi

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
  tagm=`expr $tag - 1 `
  tagm=`dtgfix3 $tagm`
done
