#!/bin/sh
f77   mettrans.f -o mettrans

echo 2 > min
echo metout >> min
echo metout.asc >> min
echo fred >> min
echo 1    >> min

./mettrans < min

rm min mettrans.o fred mettrans
if [ ! -d ../tmp ] ; then
  mkdir ../tmp
fi
cp metout ../tmp
