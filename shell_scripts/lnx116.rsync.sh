for dname in migrate nic sst_opsclimo svn python esmf viirs_ice NUOPC lands nggps cfsrr cray small usrlocal writing wigos flake tide.model drift3.paper regionalization libreoffice wcoss Pictures Desktop branch_management concentration tmp devkiss
do
  #rsync -n --delete -prtv rmg3@emc-lw-rgrumbi.ncep.noaa.gov:$dname/ $dname
  rsync --delete -prtv rmg3@emc-lw-rgrumbi.ncep.noaa.gov:$dname/ $dname
done
