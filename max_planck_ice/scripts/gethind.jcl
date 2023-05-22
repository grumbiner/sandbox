#!/bin/sh

set -x

#tag must be set externally
if [ -z $tag ] ; then
  echo Need to pass in what date to get!
  exit -1
fi
wd=`pwd`
cd $FBASE

BASE=/global/prs/

for time in 00 06 12 18
do
  rcp cray4:$BASE/flxf06.${tag}$time flxf06.${tag}$time
done

cd $wd
