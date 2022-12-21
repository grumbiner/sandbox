# QSUB -o spinbulk.out
# QSUB -lT  300
# QSUB -lM  2.0MW
# QSUB -me

cd /wd2/wd21/wd21rg/hibler/running

NCPUS=1
export NCPUS
 
#Create the restart file for Aug 30 ic.
cp ../ic/RESTARTo .

#Make the mpi directory
if [ ! -d mpibulk ]; then
  mkdir mpibulk
fi

#Make sure the stossel model is here
if [ ! -f north/stossel.north ] ; then
    cd ../mpi.source
    cp icegrid.north icegrid.inc
    chmod 644 icegrid.inc
    make
    mv stossel ../running/north/stossel.north
    cd ../running
fi
cp north/stossel.north .
cp north/MASK .
cp north/tsdeep .
cp north/tsshal .


for tag in nm17 nm16 nm15 nm14 nm13 nm12 nm11 nm10 nm9 nm8 nm7 nm6 nm5 nm4 nm3\
     nm2 nm1 nm0
do
  cp metfiles/met${tag}.north metout
  chmod 644 metout
  mpiclean

  echo /wd2/wd21/wd21rg/hibler/running/north/runparm.north > runin
  time stossel.north < runin
  mv conc conc.${tag}
  mv thick thick.${tag}
  mv FORT.16 fort16.${tag}
  mv FORT.18 fort18.${tag}
  cp RESTARTn restart.${tag}
  mv RESTARTn RESTARTo
  mv *.${tag} mpibulk

  dmput metfiles/met${tag}.north mpibulk/*.${tag} 

done 

qsub spinpip.jcl
