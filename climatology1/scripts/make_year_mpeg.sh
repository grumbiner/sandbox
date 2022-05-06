skip=1
year=1999
if [ $# -eq 1 ] ; then
  base=$1
else
  echo must provide base -- e.g. avg
  exit 1
fi

if [ -f c ] ; then
  rm c
fi
if [ -f $year ] ; then
  rm $year
fi

  count=0
  tag=${year}1001
  yp=`expr $year + 1`
  while [ $tag -lt ${yp}0931 ]
  do
    count=`expr $count + 1`
    yy=`echo $tag | cut -c1-4`
    stag=`echo $tag | cut -c5-8`

    if [ -f ${base}_$stag.gif ] ; then
      echo ${base}_$stag.gif >> c
    else
      while [ ! -f ${base}_$stag.gif ] 
      do
        tag=`expr $tag + 1`
        tag=`dtgfix3 $tag`
      done
      yy=`echo $tag | cut -c1-4`
      echo ${base}_$stag.gif >> c
    fi

    i=0
    while [ $i -lt $skip ]  
    do
      tag=`expr $tag + 1`
      tag=`dtgfix3 $tag`
      i=`expr $i + 1`
    done
  done

  echo GOP_SIZE $count >> $year
  echo OUTPUT all${year}.mpg >> $year
  echo PATTERN IPPPPPPPPPP >> $year
  echo INPUT_DIR . >> $year
  echo "INPUT_CONVERT giftopnm *" >> $year
  echo BASE_FILE_FORMAT PNM >> $year
  echo PIXEL HALF >> $year
  echo SLICES_PER_FRAME 1 >> $year
  echo REFERENCE_FRAME DECODED >> $year
  echo PQSCALE 16 >> $year
  echo IQSCALE 16 >> $year
  echo BQSCALE 16 >> $year
  echo RANGE 10 >> $year
  echo PSEARCH_ALG EXHAUSTIVE >> $year
  echo BSEARCH_ALG EXHAUSTIVE >> $year
  echo INPUT >> $year
  cat c >> $year
  echo END_INPUT >> $year


  mpeg_encode $year
