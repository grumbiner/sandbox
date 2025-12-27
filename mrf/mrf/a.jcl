#QSUB -lT 550
#QSUB -lM 8Mw

cd $DHOME/mrf
ja
  make -f makefile f6228.x
ja -chlst

