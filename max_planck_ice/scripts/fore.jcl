#QSUB -lT 90
#QSUB -me

cd /wd2/wd21/wd21rg/hibler/running/metforecast

exit

for day in `date +%h%d`
do
  for time in 12 24 36 48 60 72 84 96 108 120 132 144 156 168 
  do
    fetch ${day}.f${time} -fTB -mVX \
       -t"dsn=nmc.prod.crsfl${time}.mrf,disp=shr"
  done
  time=00
  fetch ${day}.f${time} -fTB -mVX \
     -t"dsn=nmc.prod.crsflux.t00z.fnl,disp=shr"
  dmput ${day}.f*
done
