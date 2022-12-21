#Make meteorological files
#Robert Grumbine
#prior to 13 February 1997

#QSUB -lT 25
#QSUB -lM  1Mw
#QSUB -me

cd ${RHOME}/meteo

#make clobber

chmod 644 ${IHOME}/icegrid.${pole}
cp ${IHOME}/icegrid.$pole icegrid.inc
chmod 444 ${IHOME}/icegrid.${pole}

make -i
mv wgrib met.$pole $EXDIR
make clean
