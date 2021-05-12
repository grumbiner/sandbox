#!/bin/ksh

make refill bathy toab paving
BASEDIR=/nfsuser/g01/seaspara/users/carlos/topo/

#for expt in topo_expt01.3
for expt in topo_expt01.3  topo_expt01.2  topo_expt01.1
do 
  ln -sf ${BASEDIR}/${expt}/dataout/regional.grid.a fort.061a
  ln -sf ${BASEDIR}/${expt}/dataout/regional.grid.b fort.61

  if [ ! -f fout ] ; then
    time ./refill ../shorelines/gshhs_f.b fout a.xpm > out1 2> out2 
  fi
  
  if [ $? -eq 0  -a -f fout -a ! -f bathyout ] ; then
    ./bathy fout ../topo/etopo2.raw bathyout bath.xpm 0 0 > hycom.bathy.out
  fi

  if [ -f bathyout ] ; then
    ./toab bathyout bathymask.a bathymask.b
    ./paving bathyout 40.0 330.0 paveout paver/straits
  fi 

  if [ -f paveout ] ; then
    ./toab paveout paved.a paved.b
  fi

  if [ ! -d $expt ] ; then
    mkdir $expt
  fi
  mv *.xpm fout bathyout bathymask.a bathymask.b paveout paved.a paved.b $expt
  
done
