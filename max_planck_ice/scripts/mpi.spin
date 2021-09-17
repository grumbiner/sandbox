#QSUB -o mpi.spinout
#QSUB -lT 500
#QSUB -lM    2Mw
#QSUB -me

#Spin up the sea ice model(s)
#Assume that any initial condition files needed have already
#  been created.  
#Assume too that the meteorological forcing files have also
#  already been created.

cd /tmp/wd21rg

#First, do all the of the northern hemisphere, both mpi and pips:
cp $DHOME/execs/stossel.*              .
cp $DHOME/expt.spin/restart.830   restart.north
chmod 644 restart.north

for pole in north south
do

  if [ ! -d $pole ] ; then
    mkdir $pole
  fi

  cp $HOME3/icemodel/running/$pole/ts???? .
  cp $HOME3/icemodel/running/$pole/MASK .

  for wk in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 
  do

    wkout=$wk
    export wk wkout pole

    cp /tmp/wd21rg/metouts/metout${wk}.${pole} metout
    if [ -f restart.$pole ] ; then
      cp restart.$pole RESTARTo
      echo `echo $DHOME/expt.spin/runparm.$pole` > runin
     else
      touch RESTARTo
      echo `echo $DHOME/expt.spin/runzero.$pole` > runin
    fi
    time stossel.$pole < runin
  
    mv conc         conc.${pole}.${wkout}
    mv thick        thick.${pole}.${wkout}
    mv FORT.16      fort16.${pole}.${wkout}
    mv FORT.18      fort18.${pole}.${wkout}
    mv vels         vels.${pole}.${wkout}
    mv hml          hml.${pole}.${wkout}
    mv sml          sml.${pole}.${wkout}
    mv tml          tml.${pole}.${wkout}
    mv atm.flux     atm.${pole}.${wkout}
    mv oce.flux     oce.${pole}.${wkout}
    cp RESTARTn     restart.${pole}.$wkout
    cp RESTARTn     restart.${pole}
    mv RESTARTn     RESTARTo
    mkdir ${pole}/wk${wkout}
    mv *.${wkout} ${pole}/wk${wkout}
  
  done

done

tar -cvf spinrest.tar north south
mv spinrest.tar $DHOME
#dmput $DHOME/spinrest.tar
