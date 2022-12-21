tag=19951001
count=0
skip=2
rm c
while [ $tag -lt 20060228 ]
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

echo GOP_SIZE $count
echo OUTPUT all${skip}b.mpg
echo PATTERN IPPPPPPPPPP
echo INPUT_DIR .
echo "INPUT_CONVERT giftopnm *"
echo BASE_FILE_FORMAT PNM
echo PIXEL HALF
echo SLICES_PER_FRAME 1
echo REFERENCE_FRAME DECODED
echo PQSCALE 16
echo IQSCALE 16
echo BQSCALE 16
echo RANGE 10
echo PSEARCH_ALG EXHAUSTIVE
echo BSEARCH_ALG EXHAUSTIVE
echo INPUT
cat c
echo END_INPUT
