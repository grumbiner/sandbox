CDIR=/usr1/rmg3/tapes/concentrations/ice.all/
SDIR=/usr1/sdb/sst/

tag=20010101
base=2001
wgrib $CDIR/npsg.$base > index.$base
wgrib $SDIR/sst.$base  > sindex.$base
while [ $tag -le 20010930 ] 
do
  stag=`echo $tag | cut -c3-8`
  grep $stag index.$base | wgrib -i $CDIR/npsg.$base -nh -o obs.$tag
  grep $stag sindex.$base | wgrib -i $SDIR/sst.$base -nh -o sst.$tag
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done

tag=20001001
base=2000
wgrib $CDIR/npsg.$base > index.$base
wgrib $SDIR/sst.$base  > sindex.$base
while [ $tag -le 20001231 ] 
do
  stag=`echo $tag | cut -c3-8`
  grep $stag index.$base | wgrib -i $CDIR/npsg.$base -nh -o obs.$tag
  grep $stag sindex.$base | wgrib -i $SDIR/sst.$base -nh -o sst.$tag
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done

