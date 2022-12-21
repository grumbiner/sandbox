yy=1981
while [ $yy -le 2010 ]
do
  for mm in 01 02 03 04 05 06 07 08 09 10 11 12
  do
    mkdir ${yy}${mm}
    wget  ftp://nomads.ncdc.noaa.gov/CFSR/HP_time_series/${yy}${mm}/tmp2m.gdas.${yy}${mm}.grb2
  done
  yy=`expr $yy + 1`
done

