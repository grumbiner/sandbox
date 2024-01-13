#QSUB -lT 2000
#QSUB -lM   2Mw
#QSUB -me

set -x

if [ ! -d /tmp/wd21rg/spinmet ] ; then
  mkdir /tmp/wd21rg/spinmet
fi
cd /tmp/wd21rg/spinmet

arcdir=/dm/wd21rg/forcing
exdir=/wd2/wd21/wd21rg/execs
export arcdir exdir

for pole in north 
do
  cp $HOME3/execs/stossel.$pole              .
  cp /tmp/wd21rg/spinmet/north/94112012/RESTARTn   restart.$pole
  chmod 644 restart.$pole
  if [ ! -d $pole ] ; then
    mkdir $pole
  fi
  cp $HOME3/icemodel/running/$pole/ts???? .
  cp $HOME3/icemodel/running/$pole/MASK .
  cp $HOME3/icemodel/running/$pole/run*   $pole


#Create the meteorological files:
yy=94
mo=11
dy=20 # start number a day early
export yy mo dy

fdays=0
while [ $fdays -lt 45 ] 
  do

    fdays=`expr $fdays + 1 `
#fix months, days, years here
    dy=`expr $dy + 1`
    dtg=`/home/wd21rg/dtgfix $yy $mo $dy `
    yy=`echo $dtg | cut -c1-2`
    mo=`echo $dtg | cut -c3-4`
    dy=`echo $dtg | cut -c5-6`
    
#Extract the flux files
    tar -xvf ${arcdir}/zflux.${yy}${mo}.tar \
        crsflx.${yy}${mo}${dy}00 \
        crsflx.${yy}${mo}${dy}12

    if [ ! -s crsflx.${yy}${mo}${dy}00 ] ; then
      if [ -s crsflx.${yy}${mo}${dy}12 ] ; then
        cp crsflx.${yy}${mo}${dy}12 crsflx.${yy}${mo}${dy}00
       else
        echo failed to find useable flux files for $yy $mo $dy 
        tar cvf $DHOME/spin.tar *
        exit -1
      fi
    fi
    if [ ! -s crsflx.${yy}${mo}${dy}12 ] ; then
      cp crsflx.${yy}${mo}${dy}00 crsflx.${yy}${mo}${dy}12
    fi
      
    for hh in 00 12
    do
      assign -a crsflx.${yy}${mo}${dy}$hh -s unblocked fort.51

      touch metin
      echo 1 > metin
      assign -a metout         fort.20
      time ${exdir}/met.${pole} < metin

      if [ -s restart.$pole ] ; then
        cp restart.$pole RESTARTo
        echo `echo ./$pole/runparm.$pole` > runin
       else
        touch RESTARTo
        echo `echo ./$pole/runzero.$pole` > runin
      fi
      time stossel.$pole < runin
  
      wkout=${yy}${mo}${dy}${hh}
      rm crsflx.$wkout
      mv conc         conc.${pole}.${wkout}
      mv thick        thick.${pole}.${wkout}
      mv FORT.16      fort16.${pole}.${wkout}
      mv FORT.18      fort18.${pole}.${wkout}
      mv vels         vels.${pole}.${wkout}
      mv hml          hml.${pole}.${wkout}
      mv sml          sml.${pole}.${wkout}
      mv tml          tml.${pole}.${wkout}
      mv atm.flux     atm.${pole}.${wkout}
      mv oce.flux     oce.${pole}.${wkout}
      mv metout       metout.${pole}.${wkout}
      cp RESTARTn     restart.${pole}.$wkout
      cp RESTARTn     restart.${pole}
      mv RESTARTn     RESTARTo
      mkdir ${pole}/${wkout}
      mv *.${wkout} ${pole}/${wkout}
  
    done

  done

done

tar cvf $DHOME/spin.tar *
