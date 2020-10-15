#!/bin/sh
#12 March 2014  Robert Grumbine

y=107
while [ $y -le 111 ]
do
  yy=`expr 1900 + $y`
  ./scoretoweb.pl $yy km55.0.$y/score.55.0.$y
  mv score${yy}.html scoreb${yy}
  y=`expr $y + 1`
done
