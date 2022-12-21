echo 20100112 > ein
tag4=20100112
echo $tag4 | cut -c1-2  > psin
echo $tag4 | cut -c3-4  >> psin
echo $tag4 | cut -c5-6  >> psin
echo $tag4 | cut -c7-8  >> psin


ln -f seaice_gland5min fort.11
touch fort.51
ln -f fort.51 seaice_gland5min.grib
/nwprod/exec/seaice_ice2grib5min < ein
rm fort.51
 
ln -f seaice_newland fort.11
touch fort.51
ln -f fort.51 seaice_newland.grib
/nwprod/exec/seaice_ice2grib < ein
rm fort.51

ln -f seaice_nland127.map fort.11
touch fort.51
ln -f fort.51 seaice_nland127.grib
/nwprod/exec/seaice_psgnorth12 < psin
rm fort.51

ln -f seaice_sland127.map fort.11
touch fort.51
ln -f fort.51 seaice_sland127.grib
/nwprod/exec/seaice_psgsouth12 < psin
rm fort.51

ln -f seaice_nland.map fort.11
touch fort.51
ln -f fort.51 seaice_nland.grib
/nwprod/exec/seaice_psgnorth < psin
rm fort.51

ln -f seaice_sland.map fort.11
touch fort.51
ln -f fort.51 seaice_sland.grib
/nwprod/exec/seaice_psgsouth < psin
rm fort.51

