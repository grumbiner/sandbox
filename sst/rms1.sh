DATDIR=/usr2/rmg3/reysst/reformed/
#find mean and rms difference between day and climo with trend -- traditional:
tag=19810901
j=1
while [ $tag -le 20110831 ]
do
  base=`echo $tag | cut -c5-8`
  ./rms1 $DATDIR/traditional/trad_year $DATDIR/qdoi.$tag $j 
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
  j=`expr $j + 1`
done

#Put here the version to work with monthly climatology:
#tag=19810901
##while [ $tag -le 20110831 ]
#while [ $tag -le 19810930 ]
#do
#  mo=`echo $tag | cut -c5-6`
#  ./rms trad_mon.$mo qdoi.$tag 
#  tag=`expr $tag + 1`
#  tag=`dtgfix3 $tag`
#done

#And here for doing the monthly-linear interpolation:

