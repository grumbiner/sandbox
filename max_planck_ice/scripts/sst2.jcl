#QSUB -o /eldisk1/wd21rg/sst/sst.out
#QSUB -lM 2Mw
#QSUB -lT 15
#QSUB -a "Tomorrow-8:30"
#QSUB -me

cd $HOME2/sst
qsub sst2.jcl

EXDIR=$HOME2/execs
export EXDIR

set -xe

tag=`date +"%y%m%d"`
tag=`expr $tag - 1`
tag=`dtgfix3 $tag`

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

