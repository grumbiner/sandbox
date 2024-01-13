#QSUB -lT 50
#QSUB -lM  2Mw
#QSUB -me

#Construct the sea ice forecast model itself.
#Robert Grumbine
#Last Modified 21 December 1995

cd $RHOME/mpi.source

#make clobber

chmod 644 ${IHOME}/icegrid.${pole}
cp ${IHOME}/icegrid.$pole icegrid.inc
chmod 444 ${IHOME}/icegrid.${pole}

make -i
mv stossel $EXDIR/stossel.${pole}

make clobber
