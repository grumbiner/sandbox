SDIR=$HOME3/icemodel/mpi.source
cp $SDIR/icegrid.glk $SDIR/io/icegrid.inc
cp $SDIR/icegrid.glk .
cf77 -c tsedit.f
cf77 -c $SDIR/io/BCSINIT.f 
cf77 tsedit.o BCSINIT.o -o tsedit
chmod 644 $SDIR/io/icegrid.inc
rm $SDIR/io/icegrid.inc
#cp tsedit running/glk
#cd running/glk
tsedit
