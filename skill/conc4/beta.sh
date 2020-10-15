#!/bin/sh

export tag=20160501
while [ $tag -le 20160501 ]
do
  export PDY=$tag
  time ./alpha.sh 

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
