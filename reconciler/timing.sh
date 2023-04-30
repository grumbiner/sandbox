#!/bin/ksh

make refill 
BASEDIR=/nfsuser/g01/seaspara/users/carlos/topo/

#for expt in topo_expt01.1  topo_expt01.2  topo_expt01.3
for expt in topo_expt01.3
do 
  ln -sf ${BASEDIR}/${expt}/dataout/regional.grid.a fort.061a
  ln -sf ${BASEDIR}/${expt}/dataout/regional.grid.b fort.61

  time ./refill ../shorelines/gshhs_f.b fout a.xpm > out1 2> out2 
#  gprof refill > p

done
