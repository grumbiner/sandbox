#!/bin/sh

tag=20000101
while [ $tag -le 20001231 ]
do
  stag=`echo $tag | cut -c5-8`
  ln -sf out/count.$stag climo

  grads -lcb ref.gs
  for base in count avg rms sd cond_avg cond_rms cond_sd
  do
    mv $base.gif ${base}_$stag.gif
  done

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`

done
#   0 lrwxrwxrwx. 1 rmg3 wd2      10 Oct  3 12:17 climo -> count.1231
#  64 -rw-------. 1 rmg3 wd2   64940 Oct  3 12:17 count.gif
#  64 -rw-------. 1 rmg3 wd2   64782 Oct  3 12:17 avg.gif
#  64 -rw-------. 1 rmg3 wd2   65259 Oct  3 12:17 rms.gif
#  68 -rw-------. 1 rmg3 wd2   68216 Oct  3 12:17 sd.gif
#  68 -rw-------. 1 rmg3 wd2   68209 Oct  3 12:17 cond_avg.gif
#  68 -rw-------. 1 rmg3 wd2   67679 Oct  3 12:17 cond_rms.gif
#  68 -rw-------. 1 rmg3 wd2   68453 Oct  3 12:17 cond_sd.gif
