#QSUB -lT 600
#QSUB -lM 8Mw

cd $HOME3/mrf
ja
  make -f makefile f6228.tst.x
ja -chlst

