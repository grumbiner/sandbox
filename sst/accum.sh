#!/bin/sh


OUTDIR=/u/wx21rg/noscrub/avhrr/

tag=20090201
tagm=`expr $tag - 1`
tagm=`/u/wx21rg/bin/dtgfix3 $tagm`

tmp=`date +"%Y%m%d"`
tmp=`expr $tmp - 1`
tmp=`/u/wx21rg/bin/dtgfix3 $tmp`


while [ $tag -le $tmp ]
do

  if [ ! -f $OUTDIR/accout.$tag ] ; then
    ./accumulate $OUTDIR/fillsst.$tagm $OUTDIR/sstout.$tag $OUTDIR/age.$tagm \
                 $OUTDIR/fillsst.$tag   $OUTDIR/age.$tag > $OUTDIR/accout.$tag
    convert -flip fill.xpm $OUTDIR/fill.$tag.png
    rm fill.xpm
  fi
 
  tagm=$tag
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
