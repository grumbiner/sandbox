#QSUB -lT 2000
#QSUB -lM   4Mw
#QSUB -me

if [ ! -d /tmp/wd21rg/spinmet ] ; then
  mkdir /tmp/wd21rg/spinmet
fi

cd /tmp/wd21rg/spinmet

arcdir=/dm/wd21rg/forcing
exdir=/dm/wd21rg/execs
export arcdir exdir

#Create the meteorological files:
yy=93
mo=08
dy=26 # start number a day early
export yy mo dy

wk=0
while [ $wk -lt 18 ] 
  do
  wk=`expr $wk + 1 `
  i=50
  for fd in 0 1 2 3 4 5 6
  do

#fix months, days, years here
    dy=`expr $dy + 1`
    dtg=`/dm/wd21rg/expt.spin/dtgfix $yy $mo $dy `
    yy=`echo $dtg | cut -c1-2`
    mo=`echo $dtg | cut -c3-4`
    dy=`echo $dtg | cut -c5-6`
    
#Extract the flux files
    tar -xvf ${arcdir}/zflux.${yy}${mo}.tar \
        crsflx.${yy}${mo}${dy}00 \
        crsflx.${yy}${mo}${dy}12
    i=`expr $i + 1`
    assign -a crsflx.${yy}${mo}${dy}00 -s unblocked fort.$i
    i=`expr $i + 1`
    assign -a crsflx.${yy}${mo}${dy}12 -s unblocked fort.$i
  done

  echo week $wk , yymmdd = ${yy}${mo}${dy}
  for pole in north south glk
  do
    touch metin
    echo 14 > metin
    assign -a metout${wk}.${pole} fort.20
    time ${exdir}/met.${pole} < metin
  done
done

for pole in north south glk
do
  tar -cvf ${arcdir}/${pole}.tar metout${wk}.$pole
done
