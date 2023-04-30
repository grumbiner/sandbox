#!/bin/sh
#set -x
#
#EXDIR=/marine/save/rgdev/satellites/amsr/exec/
EXDIR=/nwprod/exec/
WORKDIR=/stmp/wx21rg/amsre.dew
DATADIR=/marine/noscrub/wx21rg/amsr.dew
#DATADIR=/marine/noscrub/wx21rg/diane/amsre2
OUTDIR=/marine/noscrub/wx21rg/amsr.dew
FIXDIR=/marine/save/rgdev/satellites/amsr/fix/
OPSFIXDIR=/nwprod/fix/
############################
##
if [ ! -d $WORKDIR ] ; then
  mkdir -p $WORKDIR
fi
if [ ! -d $OUTDIR ] ; then
  mkdir -p $OUTDIR
fi
cd $WORKDIR

export XLFRTEOPTS="unit_vars=yes"

if [ ! -f seaice_TBccant.tab.amsr ] ; then
  cp $FIXDIR/* .
fi

tag=20090501
while [ $tag -le 20090713 ]
do
  CDATE=$tag

  if [ -f $DATADIR/amsre.$tag ] ; then
    export XLFUNIT_11=$DATADIR/amsre.$tag
    timex $EXDIR/seaice_amsrbufr > ft06.$CDATE 2>>err.$CDATE
    mv fort.51 output.$CDATE
  fi

  if [ -f output.$CDATE ] ; then
    timex $EXDIR/seaice_seaiamsr output.$CDATE \
           /nwprod/fix/seaice_nland127.map /nwprod/fix/seaice_sland127.map \
           nraw.$CDATE sraw.$CDATE nchar.$CDATE schar.$CDATE > iceout.$CDATE
  else
    touch output.$CDATE
    echo THERE WAS NO AMSR DATA FOR $CDATE
    timex $EXDIR/seaice_seaiamsr output.$CDATE \
           /nwprod/fix/seaice_nland127.map /nwprod/fix/seaice_sland127.map \
           nraw.$CDATE sraw.$CDATE nchar.$CDATE schar.$CDATE > iceout.$CDATE
  fi

  if [ -f nchar.$CDATE ] ; then
    $EXDIR/seaice_north12xpm nchar.$CDATE $OPSFIXDIR/seaice_nland127.map n$CDATE.xpm
    $EXDIR/seaice_south12xpm schar.$CDATE $OPSFIXDIR/seaice_sland127.map s$CDATE.xpm
  fi

  if [ -f nchar.$CDATE ] ; then
    mv nchar.$CDATE schar.$CDATE $OUTDIR
  fi
  if [ -f n$CDATE.xpm ] ; then
    mv [ns]$CDATE.xpm $OUTDIR
  fi


  if [ -f output.$CDATE ] ; then
    rm output.$CDATE
  fi
  
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`

done
