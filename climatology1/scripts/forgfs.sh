#!/bin/sh

tag=19800101
while [ $tag -le 19801231 ]
do
  days=`echo $tag | cut -c5-8`
  ./forgfs count.$days low.$days high.$days

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done

