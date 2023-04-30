skip=1
tag=19951001
year=all
  count=0
  rm c
while [ $tag -le 20090931 ]
do
  
    count=`expr $count + 1`
    yy=`echo $tag | cut -c1-4`

    if [ -f ../nh/$yy/nh.$tag.gif ] ; then
      echo ../nh/$yy/nh.$tag.gif >> c
    else
      while [ ! -f ../nh/$yy/nh.$tag.gif ] 
      do
        tag=`expr $tag + 1`
        tag=`dtgfix3 $tag`
      done
      yy=`echo $tag | cut -c1-4`
      echo ../nh/$yy/nh.$tag.gif >> c
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

