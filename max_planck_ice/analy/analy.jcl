#QSUB -lT 20
#QSUB -lM 1Mw
#QSUB -me

#File to make the analysis support programs.
#Robert Grumbine
#Last Modified 21 December 1995.

cd $RHOME/analy

chmod 644 ${IHOME}/icegrid.${pole}
cp ${IHOME}/icegrid.$pole icegrid.inc
chmod 444 ${IHOME}/icegrid.${pole}

make -i
mv mag     mag.$pole
mv mapcont mapcont.$pole
mv corners corners.$pole
mv image   image.$pole
mv *.$pole $EXDIR
make clean
