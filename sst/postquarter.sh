#!/bin/bash

set -xe

export RUNDIR=/ptmp/wx21rg/climo
export SRCDIR=~/noscrub/sst/process
export FIXDIR=~/rgdev/masks/

if [ $SRCDIR/postquarter.C -nt postquarter ] ; then
  ncepxlC -O2 $SRCDIR/postquarter.C -DIBM -I ~/noscrub/ice/source/includes \
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

#if [ ! -f tcount ] ; then
  cp count tcount
#fi

./postquarter tcount newquarter tmpstats
