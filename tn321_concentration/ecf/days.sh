#!/bin/bash --login

export tag=20210413
tagm=`expr $tag - 1`
export tagm=`/u/Robert.Grumbine/bin/dtgfix3 $tagm`
export end=20210419
echo initial tag date = $tag

#-----------------------------------------------------------------------------
set -xe
#--------------------------------------------------------------------------------------
#The actual running of stuff

while [ $tag -le $end ]
do
  ./day.sh > out.$tag

  tag=`expr $tag + 1`
  tag=`/u/Robert.Grumbine/bin/dtgfix3 $tag`
done
