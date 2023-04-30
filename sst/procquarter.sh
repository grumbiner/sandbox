#!/bin/sh

set -x

REYDIR=/marine/noscrub/wx21rg/sst/reynolds/
RUNDIR=/ptmp/wx21rg/climo
OUTDIR=/marine/noscrub/wx21rg/sst/reynolds/
export SRCDIR=/marine/save/rgdev/sst/
export FIXDIR=/marine/save/rgdev/masks/
export INCDIR=$SRCDIR/include

export REYDIR RUNDIR OUTDIR

START=19850104
END=19860103

#Should need no changes below here ----------------------------


if [ ! -d $RUNDIR ] ; then
  mkdir -p $RUNDIR
fi


ncepxlf -O2 $SRCDIR/read.f -o sstread
ncepxlC $SRCDIR/sstquarter.C -I $INCDIR -O2 -DIBM \
                  -L /nwprod/lib/ -l ombf_4 -o sstquarter 
ncepxlC $SRCDIR/processquarter.C -I $INCDIR -O2 -DIBM \
                  -L /nwprod/lib/ -l ombf_4 -o processquarter

if [ ! -f sstread -o ! -f sstquarter -o ! -f processquarter ] ; then
  echo could not compile needed file
  exit 1
fi
cp sstread sstquarter processquarter $RUNDIR

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
  if [ ! -d $yy ] ; then
    if [ ! -f $yy.tar ] ; then
      if [ -f $REYDIR/$yy.tar ] ; then
        cp $REYDIR/$yy.tar .
        tar xvf $yy.tar
      elif [ -f $REYDIR/$yy.tar.gz ] ; then
        cp $REYDIR/$yy.tar.gz .
        gunzip $yy.tar.gz
        tar xvf $yy.tar
      else
        echo could not deal with year $yy, stopping
        ls -l $REYDIR
        exit 1
      fi
    else 
      tar xvf $yy.tar
    fi 
  fi

#Read in from reynolds format, write out deg C
  if [ ! -f sst.$tag ] ; then 
    if [ -f  ${yy}/sst4-path-eot.$tag ] ; then
      ln -sf ${yy}/sst4-path-eot.$tag sst
      ./sstread > read.$tag
      mv sst.out sst.$tag
    elif [ -f ${yy}/sst4-navy-eot.$tag ] ; then
      ln -sf ${yy}/sst4-navy-eot.$tag sst
      ./sstread > read.$tag
      mv sst.out sst.$tag
    else
      echo no sst for $tag
    fi
  fi

#Translate to K, on my 0.25 degree grid
  if [ ! -f sstquarter.$tag ] ; then
    if [ -f  sst.$tag ] ; then
      ./sstquarter sst.$tag sstquarter.$tag > quarterout.$tag
    else
      echo no sst_quarter for $tag
    fi
  fi

#Make the running count of statistics:
  if [ -s sstquarter.$tag ] ; then
    if [ -s count ] ; then
      ./processquarter  sstquarter.$tag count > proc.$tag
      if [ $? -eq 0 ] ; then
        rm sstquarter.$tag sst.$tag
      fi
    else
      ./processquarter  sstquarter.$tag       > proc.$tag
    fi
    tally=`expr $tally + 1`
  fi

  rem=`echo $tag | cut -c5-8`
  if [ $rem = 0103 ] ; then
    cp count $OUTDIR/count.$yy
    ym=`expr $yy \- 1`
    if [ -f $ym.tar ] ; then
      rm -rf $ym
    fi
  fi

  tag=`expr $tag + 1`
  tag=`/u/wx21rg/bin/dtgfix3 $tag`

done 

echo found $tally days of sst data


# Now do the post-processing to create the posteriori files and the
#   climatology files.

if [ $SRCDIR/postquarter.C -nt postquarter ] ; then
  ncepxlC -O2 $SRCDIR/postquarter.C -DIBM -I $INCDIR \
                  -L /nwprod/lib/ -l ombf_4 -o postquarter
fi
if [ ! -f postquarter ] ; then
  echo failed to construct the postquarter executable
fi

if [ `pwd` != $RUNDIR ] ; then
  cp postquarter $RUNDIR
  cd $RUNDIR
fi
if [ ! -f newquarter ] ; then
  cp $FIXDIR/newquarter .
fi

cp count tcount

./postquarter tcount newquarter tmpstats
cp newquarter tmpstats $OUTDIR
