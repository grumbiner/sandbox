#!/bin/ksh

#set -x

TMPDIR=/usr1/dataflow/ice/tmp
if [ ! -d $TMPDIR ] ; then
  mkdir -p $TMPDIR
fi
cd $TMPDIR
ARCDIR=/usr1/dataflow/
EXDIR=/usr1/home/wd21rg/newice/execs/
DATDIR=/usr1/home/wd21rg/fix/
OUTDIR=/usr1/dataflow/unblock/
export DATDIR ARCDIR EXDIR TMPDIR

#
#Should not need any changes below this point -----------------------
#
for clear in fort.51 fort.14
do
  if [ -f $clear ] ; then
    rm $clear
  fi
done 

for execs in seaice_ssmibufr seaice_seaissmi 
do
  cp $EXDIR/$execs .
done

start=20040512
end=20040518
tag=$start

if [ -f fort.14 ] ; then
  rm fort.14
fi

while [ $tag -le $end ] 
do
  for clear in fort.51 fort.14
  do
    if [ -f $clear ] ; then
      rm $clear
    fi
  done

  #Get and process the bufr tank into something readable
  if [ ! -f $ARCDIR/ssmi85/ssmi85.$tag ] ; then
    echo Could not find ssmi85.$tag 
  else
    ln -sf $ARCDIR/ssmi85/ssmi85.$tag fort.14
  fi
  if [ ! -f fort.14 ] ; then
    echo failed to construct an ssmi input file for $tag
  fi
  time ./seaice_ssmibufr
  mv fort.51 $OUTDIR/unblock.$tag

  tag=`expr $tag + 1`
  tag=`/usr1/home/wd21rg/bin/dtgfix3 $tag`
done 
