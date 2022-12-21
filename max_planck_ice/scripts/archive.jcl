#QSUB -lT 900
#QSUB -me

cd /wd2/wd21/wd21rg/hibler/running/metforecast

for day in Jan25
do
  for time in 12 24 36 48 60 72 84 96 108 120 
  do
    fetch ${day}.f${time} -fTB -mVX \
       -t"dsn=usr.wd23.mrf.${day}.f${time}.flx,disp=shr"
  done
  time=00
  fetch ${day}.f${time} -fTB -mVX \
     -t"dsn=usr.wd23.gdas.${day}.crsflux.T${time}Z,disp=shr"
  time=12
  fetch ${day}.f${time}.gdas -fTB -mVX \
     -t"dsn=usr.wd23.gdas.${day}.crsflux.T${time}Z,disp=shr"
  dmput ${day}.f*
done
