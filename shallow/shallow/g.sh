#!/bin/zsh

make -f m2

if [ $# -ne 0 ] ; then
  procs=$1
else
  procs=1
fi

time mpirun -np $procs ./global > stdout 2> stderr

for f in *.xpm
do
  convert $f $f.gif
  rm $f
done
