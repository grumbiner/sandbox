#!/bin/bash

#set -xe

tag=20000201
rm -f assel.ctl

while [ $tag -le 20031231 ]
do
  if [ -f assel/glob$tag.bin ] ; then
    echo \'open assel.ctl \' > mask3.gs
    echo \'set gxout grfill \' >> mask3.gs
    echo \'set display color white \' >> mask3.gs
    echo \'clear \' >> mask3.gs
    echo \'set grads off \' >> mask3.gs
    echo \'enable print c.grads \' >> mask3.gs
    echo \'set mpdset hires \' >> mask3.gs
    echo \'set lat  40  51 \' >> mask3.gs
    echo \'set lon -94 -75 \' >> mask3.gs
    echo \'set cmax 224\' >> mask3.gs
    echo \'set strsiz 0.2\' >> mask3.gs
    echo \'draw string 3.75 7.75 GLERL Ice Cover $tag \' >> mask3.gs
    echo \'d icec \' >> mask3.gs
    echo \'run cbarn \' >> mask3.gs
    echo \'print  \' >> mask3.gs
    echo \'quit \' >> mask3.gs

    echo dset assel/glob$tag.bin > assel.ctl
    echo options yrev template  >> assel.ctl
    echo undef 224.             >> assel.ctl
    echo title land mask        >> assel.ctl
    echo xdef  4320 linear   0.041667   0.083333333 >> assel.ctl
    echo ydef  2160 linear -89.958333  +0.083333333 >> assel.ctl
    echo zdef 1 levels 1                            >> assel.ctl
    echo tdef       12 linear 00Z24Oct03 1dy >> assel.ctl
    echo vars 1                              >> assel.ctl
    echo ICEC     0  91 ,102,0      Ice concentration  >> assel.ctl 
    echo endvars >> assel.ctl

    grads -lcb mask3.gs
    gxeps -c -i c.grads -o a.ps
    convert -rotate 90 a.ps assel${tag}.png
  fi

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
