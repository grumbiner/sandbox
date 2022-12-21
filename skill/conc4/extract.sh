hh=00
while [ $hh -le 384 ]
do

  if [ -f gfs.t00z.sfluxgrbf${hh} ] ; then
    ln -sf gfs.t00z.sfluxgrbf${hh} gfsin

    wgrib gfsin > index
    for parm in UFLX VFLX ICEC ICETK PRATE CPRAT\
     TMP:kpds5=11:kpds6=105:kpds7=2\
     UGRD:kpds5=33:kpds6=105:kpds7=10\
     VGRD:kpds5=34:kpds6=105:kpds7=10\
     SPFH:kpds5=51:kpds6=105:kpds7=2\
     PRES:kpds5=1:kpds6=1:kpds7=0   \
     GFLUX:kpds5=155:kpds6=1:kpds7=0:TR=3 \
     SHTFL:kpds5=122:kpds6=1:kpds7=0:TR=3 \
     LHTFL:kpds5=121:kpds6=1:kpds7=0:TR=3  \
     DLWRF:kpds5=205:kpds6=1:kpds7=0:TR=3\
     DSWRF:kpds5=204:kpds6=1:kpds7=0:TR=3
    do
      grep $parm index | wgrib -i gfsin -append -o ftnout.$hh
    done
  fi

  hh=`expr $hh + 1`
  if [ $hh -lt 10 ] ; then
    hh=0$hh
  fi

done
