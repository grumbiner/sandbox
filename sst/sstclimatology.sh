#!/bin/sh


RUNDIR=~/sstclimo/running
REYDIR=/usr2/rmg3/reysst/reformed
OUTDIR=/usr2/rmg3/reysst/
FIXDIR=/usr2/rmg3/allbathy/products
EXDIR=~/svn/sstclimo/

export REYDIR RUNDIR OUTDIR FIXDIR EXDIR

START=19810901
END=19850831

#Should need no changes below here ----------------------------
if [ ! -d $RUNDIR ] ; then
  mkdir -p $RUNDIR
fi
cd $RUNDIR

if [ -f count ] ; then
  rm count
fi
if [ -f tmpsst ] ; then
  rm tmpsst
fi

tally=0
tag=$START

while [ $tag -le $END ]
do
  yy=`echo $tag | cut -c1-4`

#Make the running count of statistics:
  if [ -s $REYDIR/qdoi.$tag ] ; then
    if [ -s count ] ; then
      $EXDIR/processquarter  $REYDIR/qdoi.$tag count > proc.$tag
    else
      $EXDIR/processquarter  $REYDIR/qdoi.$tag       > proc.$tag
    fi
    #tally is number of data files found
    tally=`expr $tally + 1`
  fi

  rem=`echo $tag | cut -c5-8`
  if [ $rem = "0831" ] ; then
    cp count $OUTDIR/count.$yy
    ym=`expr $yy \- 1`
  fi

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`

done 

echo found $tally days of sst data


# Now do the post-processing to create the posteriori files and the
#   climatology files.

set -x

if [ ! -f newquarter ] ; then
  cp $FIXDIR/master.quarter newquarter
fi
cp count tcount

$EXDIR/postquarter tcount newquarter tmpstats
cp tmpstats $OUTDIR
