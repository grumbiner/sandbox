#!/bin/sh
for d in accumulate.cd compare.cd scan.cd update.cd
do
  cd $d
  make
  cd ..
done
