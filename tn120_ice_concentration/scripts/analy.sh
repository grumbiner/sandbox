#!/bin/bash

#set -xe

tag=20000201

while [ $tag -le 20031231 ]
do
  if [ -f hires.output/fill5min.$tag ] ; then
    ./tofloat hires.output/fill5min.$tag revised.flags analy
    echo \'open mask.ctl \' > mask4.gs
    echo \'set gxout grfill \' >> mask4.gs
    echo \'set display color white \' >> mask4.gs
    echo \'clear \' >> mask4.gs
    echo \'set grads off \' >> mask4.gs
    echo \'enable print c.grads \' >> mask4.gs
    echo \'set mpdset hires \' >> mask4.gs
    echo \'set lat  60  68 \' >> mask4.gs
    echo \'set lon -126 -108 \' >> mask4.gs
    echo \'set cmax 224\' >> mask4.gs
    echo \'set strsiz 0.2\' >> mask4.gs
    echo \'draw string 3.75 7.75 5 Minute Ice Cover $tag \' >> mask4.gs
    echo \'d icec \' >> mask4.gs
    echo \'run cbarn \' >> mask4.gs
    echo \'print  \' >> mask4.gs
    echo \'quit \' >> mask4.gs
    grads -lcb mask4.gs
    gxeps -c -i c.grads -o d.ps
    convert -rotate 90 d.ps bear${tag}.png
  fi

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
