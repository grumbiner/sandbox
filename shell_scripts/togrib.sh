for mo in 01 02 03 04 05 06 07 08 09 10 11 12
do
  echo 1996${mo}15 > tag
  ln -sf ${mo}.txt fort.11
  cat tag | ./seaice_ice2grib5min > ${mo}.out
  mv fort.51 ${mo}.grib
done
