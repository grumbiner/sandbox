# QSUB -o spinsouth.out
# QSUB -lT  300
# QSUB -lM  2.0MW
# QSUB -me

cd /wd2/wd21/wd21rg/hibler/running

NCPUS=1
export NCPUS
 
#Make the mpi directory
if [ ! -d mpisouth ]; then
  mkdir mpisouth
fi

#Make sure the stossel model is here
if [ ! -f south/stossel.south ] ; then
    cd ../mpi.source
    cp icegrid.south icegrid.inc
    chmod 644 icegrid.inc
    make
    mv stossel ../running/south/stossel.south
    cd ../running
fi
cp south/stossel.south .
cp south/MASK .
cp south/tsdeep .
cp south/tsshal .

#Must start from zero condition as no satellite was saved.
for tag in nm17 
do
  cp metfiles/met${tag}.south metout
  chmod 644 metout
  mpiclean
  touch RESTARTo

  echo /wd2/wd21/wd21rg/hibler/running/south/runzero.south > runin
  time stossel.south < runin
  mv conc conc.${tag}
  mv thick thick.${tag}
  mv FORT.16 fort16.${tag}
  mv FORT.18 fort18.${tag}
  cp RESTARTn restart.${tag}
  mv RESTARTn RESTARTo
  mv *.${tag} mpisouth

  dmput metfiles/met${tag}.south mpisouth/*.${tag} 

done 

# Now can run heel on toe to the end of 1993.
for tag in nm16 nm15 nm14 nm13 nm12 nm11 nm10 nm9 nm8 nm7 nm6 nm5 nm4 nm3\
     nm2 nm1 nm0
do
  cp metfiles/met${tag} metout
  mpiclean

  echo south/runparm.south > runin
  time stossel.south < runin
  mv conc conc.${tag}
  mv thick thick.${tag}
  mv FORT.16 fort16.${tag}
  mv FORT.18 fort18.${tag}
  cp RESTARTn restart.${tag}
  mv RESTARTn RESTARTo
  mv *.${tag} mpisouth

  dmput metfiles/met${tag}.south mpisouth/*.${tag} 

done 

#qsub spinbulk.jcl
