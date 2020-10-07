#!/bin/sh

./plot fout

frames=250
i=0
if [ -f rho ] ; then
  rm rho
fi
if [ -f u ] ; then
  rm u
fi
if [ -f v ] ; then
  rm v
fi
if [ -f zeta ] ; then
  rm zeta
fi

while [ $i -lt $frames ]
do
  if [ -f rho$i.xpm ] ; then
    convert rho$i.xpm rho$i.png
    echo rho$i.png >> rho
    rm rho$i.xpm
  fi
  if [ -f u$i.xpm ] ; then
    convert u$i.xpm u$i.png
    echo u$i.png >> u
    rm u$i.xpm
  fi
  if [ -f v$i.xpm ] ; then
    convert v$i.xpm v$i.png
    echo v$i.png >> v
    rm v$i.xpm
  fi
  if [ -f zeta$i.xpm ] ; then
    convert zeta$i.xpm zeta$i.png
    echo zeta$i.png >> zeta
    rm zeta$i.xpm
  fi

  i=`expr $i + 1`
done 

#whirlgif -o rho_anim.gif -i rho 
#whirlgif -o u_anim.gif -i u 
#whirlgif -o v_anim.gif -i v
#whirlgif -o zeta_anim.gif -i zeta
