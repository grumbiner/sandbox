for n in 1 2 3 4 5
do
  cd wwwdm${n}.ftp.ncep.noaa.gov
  mkdir tmp
  mv *gz tmp
  cd tmp
  gunzip *gz
  find . -exec dcmp {} ../ \;
  cd ../..
done
