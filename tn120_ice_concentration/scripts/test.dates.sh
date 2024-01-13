tag=950901

while [ $tag -le 990831 ]
do
  base=`echo $tag | cut -c1-4`
  dd=`echo $tag | cut -c5-6`

    if [ ! -f nindex.$base ] ; then
      wgrib npsg.${base} > nindex.$base
    fi
    if [ ! -f sindex.$base ] ; then
      wgrib spsg.${base} > sindex.$base
    fi

  grep $tag nindex.$base | wgrib -i npsg.${base} -o nout.$tag
  grep $tag sindex.$base | wgrib -i spsg.${base} -o sout.$tag
  if [ ! -s nout.$tag ] ; then
    echo failed on north $tag >> bad.dates
  fi
  if [ ! -s sout.$tag ] ; then
    echo failed on south $tag >> bad.dates
  fi
    
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`

done     
