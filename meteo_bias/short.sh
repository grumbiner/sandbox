#!/bin/sh

ddir=/ptmp/wx21rg/
tag=20021101

#set -x

cd $ddir
cp /nfsuser/g01/wx21rg/test/explore/explore .

while [ $tag -le 20021112 ]
do
  for hh in 00 06 12 18
  do
    tagh=${tag}$hh
    if [ -f north.$tagh -a -f south.$tagh -a \
         -f nssmi.$tagh -a -f sssmi.$tagh ] ; then  
      time ./explore north.$tagh $ddir/nssmi.$tagh nout.$tagh \
                  south.$tagh $ddir/sssmi.$tagh sout.$tagh \
         >> summary.explore 
    fi
  done
  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
