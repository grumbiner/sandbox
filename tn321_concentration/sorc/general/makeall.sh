#!/bin/sh

export MMAB_VER=${MMAB_VER:-3.5.0}
export BASE=${BASE:-${MMAB_BASE}}

for d in seaice_blend.Cd seaice_edge.Cd seaice_filtanal.Cd seaice_ice2grib.fd seaice_icegrid.Cd seaice_posteriori.Cd seaice_psgnorth.fd seaice_psgsouth.fd seaice_reduce.Cd seaice_xpm.Cd seaice_regions.Cd seaice_monitor.Cd seaice_grib2.fd seaice_imsice.Cd seaice_noice.Cd
do
  cd $d
  make
  cd ..
done

