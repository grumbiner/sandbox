REANL=$HOME3/execs
SDIR=$DHOME/ssmi/fifteenth

cd /export/s2/reanl

#set -x

tag=960314
tagm=`expr $tag - 1 `
tagm=`dtgfix3 $tagm`

while [ $tag -le 960317 ] 
do

  yy=`echo $tag | cut -c1-2`

#Get the operational SST field
  if [ -f dump ] ; then
    rm dump
  fi
  grep $tag sst/sst.19$yy.index | wgrib sst/sst.19$yy -i 
  stag=$tagm
  while [ ! -s dump ] 
  do
    grep $stag sst/sst.19$yy.index | wgrib sst/sst.19$yy -i 
    stag=`expr $stag - 1`
    stag=`dtgfix3 $stag`
    if [ $stag -lt 951001 ] ; then
      echo back tracked on sst to october 1 and found no file!
      exit -1
    fi
  done
  if [ ! -s dump ] ; then
    echo no sst file on $tag  quitting
    exit -1
  fi

    
  echo starting filtbg2
#Filter the ice concentration field (in lat-long space only so far)
  ./filtbg2 dump ice/umasknorth.$tag ice/umasksouth.$tag latlon.$tag \
                    nps.$tag sps.$tag 275.2
  echo passed filtbg2
#fill in the ice concentration field with older data as required
  ./tfill fill.$tagm latlon.$tag age.$tagm age.$tag fill.$tag
  echo passed tfill


  if [ -s latlon.$tag ] ; then
#    rm ice/?3ssmi.$tag umask*.$tag ?ps.$tag ?3ssmi.$tag
    rm ?ps.$tag
    echo $tag
   else
    echo failed to generate latlon file! $tag
    exit -1
  fi

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
  tagm=`expr $tag - 1 `
  tagm=`dtgfix3 $tagm`

done
