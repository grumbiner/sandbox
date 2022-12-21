#!/bin/sh

export tag=20160417
while [ $tag -lt 20160501 ]
do
  export PDY=$tag
  time ./scoreall.sh 

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done

export PDY=20160519
export tag=$PDY
time ./scoreall.sh 
