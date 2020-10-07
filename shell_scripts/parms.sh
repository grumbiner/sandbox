tag=20031001
while [ $tag -le 20040930 ]
do
  yy=`echo $tag | cut -c1-4`
  for hh in 00 06 12 18
  do
    ltag=$tag${hh}
    fn=/usr1/data/3d/fluxes/$yy/flxf06.$ltag
    if [ ! -f out.$ltag ] ; then
      if [ -f $fn ] ; then
      wgrib $fn > index
        for parm in SHTFL LHTFL TMP:kpds5=11:kpds6=1: DLWRF:kpds5=205 \
                ULWRF:kpds5=212:kpds6=1 \
                USWRF:kpds5=211:kpds6=1 DSWRF:kpds5=204 PRATE UGRD VGRD \
                SPFH PRES:kpds5=1:kpds6=1: HPBL ALBDO LAND ICEC ICETK
        do
          grep $parm index | wgrib -i $fn -o out.$ltag -append
        done
        #want to extract point of interest here for the met file in simple input
      fi
    fi
    #need an else clause to manage missing days
  done

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done

