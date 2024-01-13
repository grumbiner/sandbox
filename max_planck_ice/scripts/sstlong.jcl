ja
cd $HOME2/sst

EXDIR=$HOME2/execs
export EXDIR

set -xe

tag=`date +"%y%m%d"`
tag=`expr $tag - 1`
tag=`dtgfix3 $tag`
for tag in 970225 970226 970227 970228 970301 970302 970303 970304
do
 
#assign -a /cray3_com/fnl/PROD/sst.$tag/sst.T12Z.eta_grid -Fcos -Nibm fort.10
assign -a /scom/fnl/prod/sst.$tag/sst.T12Z.eta_grid -Fcos -Nibm fort.10
assign -a sst.$tag.grib -s unblocked fort.12
assign -a sst.glk.$tag.grib -s unblocked fort.13

echo $tag | $EXDIR/sst14c

#$EXDIR/wgrib sst.$tag.grib | $EXDIR/wgrib -i sst.$tag.grib -nh -o sstin 
#assign -a sstin -s unblocked fort.10
#assign -a sstout fort.11
#if [ -f sstout ] ; then
#  rm sstout
#fi
#$EXDIR/sstprog 
#mv sstout sst.glk.$tag
#

done
ja -chlst
