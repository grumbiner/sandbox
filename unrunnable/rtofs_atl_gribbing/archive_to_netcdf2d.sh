#!/bin/csh
                                                                                                                                                                       
# prepare archv23z
setenv execnc /nfsuser/g01/seaspara/users/avichal/archiv_for_grib/exec
echo $execnc
                                                                                                                                                                       
setenv  INPUTDIR    /nfsuser/g01/seaspara/users/avichal/archiv_for_grib/inputs
setenv  WORKDIR     /gpfstmp/seaspara/netcdf
setenv  ARCHDIR     /gpfs/f/ptmp/seaspara/avichal/nowcast
                                                                                                                                                                       
setenv DEPTHFILEa  ${INPUTDIR}/regional.depth.a
setenv DEPTHFILEb  ${INPUTDIR}/regional.depth.b
                                                                                                                                                                       
setenv GRIDFILEa   ${INPUTDIR}/regional.grid.a
setenv GRIDFILEb   ${INPUTDIR}/regional.grid.b
                                                                                                                                                                       
echo WORKDIR  =   $WORKDIR
echo $DEPTHFILEa
echo $GRIDFILEa
mkdir -p $WORKDIR
cd $WORKDIR
                                                                                                                                                                       
touch regional.grid.a regional.grid.b
/bin/rm regional.grid.a regional.grid.b
ln -s ${GRIDFILEa} regional.grid.a
ln -s ${GRIDFILEb} regional.grid.b
                                                                                                                                                                       
cp $ARCHDIR/archv.2005_199_00.b  archv.b
cp $ARCHDIR/archv.2005_199_00.a  archv.a

cd $WORKDIR

touch archv2ncdf2d.in
/bin/rm archv2ncdf2d.in

cat >  archv2ncdf2d.in<<EOF
archv.b
NetCDF
000	'iexpt ' = experiment number x10 (000=from archive file)
  0	'yrflag' = days in year flag (0=360J16,1=366J16,2=366J01,3-actual)
 1200	'idm   ' = longitudinal array size
 1684	'jdm   ' = latitudinal  array size
 25	'kdm   ' = number of layers
 34.0	'thbase' = reference density (sigma units)
  0	'smooth' = smooth fields before plotting (0=F,1=T)
  0	'mthin ' = mask thin layers from plots   (0=F,1=T)
  1	'iorign' = i-origin of plotted subregion (226 for Isabel)
  1	'jorign' = j-origin of plotted subregion (428 for Isabel)
  0	'idmp  ' = i-extent of plotted subregion (<=idm; 0 implies idm) (626 for Isabel)
  0	'jdmp  ' = j-extent of plotted subregion (<=jdm; 0 implies jdm) (485 for Isabel)
  0     'botio ' = bathymetry       I/O unit (0 no I/O)
  0     'flxio ' = surf. heat flux  I/O unit (0 no I/O)
  0     'empio ' = surf. evap-pcip  I/O unit (0 no I/O)
  0     'evpio ' = surf. evaporatv  I/O unit (0 no I/O)
  0     'snsio ' = surf. sensiblht  I/O unit (0 no I/O)
  0     'ttrio ' = surf. temp trend I/O unit (0 no I/O)
  0     'strio ' = surf. saln trend I/O unit (0 no I/O)
  0     'icvio ' = ice coverage     I/O unit (0 no I/O)
  0     'ithio ' = ice thickness    I/O unit (0 no I/O)
  0     'ictio ' = ice temperature  I/O unit (0 no I/O)
 23     'sshio ' = sea surf. height I/O unit (0 no I/O)
  0     'bsfio ' = baro. strmfn.    I/O unit (0 no I/O)
  0     'uvmio ' = mix. lay. u-vel. I/O unit (0 no I/O)
  0     'vvmio ' = mix. lay. v-vel. I/O unit (0 no I/O)
  0     'spmio ' = mix. lay. speed  I/O unit (0 no I/O)
  0     'bltio ' = bnd. lay. thick. I/O unit (0 no I/O)
 24     'mltio ' = mix. lay. thick. I/O unit (0 no I/O)
  0     'sstio ' = mix. lay. temp.  I/O unit (0 no I/O)
  0     'sssio ' = mix. lay. saln.  I/O unit (0 no I/O)
  0     'ssdio ' = mix. lay. dens.  I/O unit (0 no I/O)
-01	'kf    ' = first output layer (=0 end output; <0 label with layer #)
 01	'kl    ' = last  output layer
 54     'uvlio ' = layer k   u-vel. I/O unit (0 no I/O)
 55     'vvlio ' = layer k   v-vel. I/O unit (0 no I/O)
  0     'splio ' = layer k   speed. I/O unit (0 no I/O)
  0     'iwvio ' = layer k   i.vel. I/O unit (0 no I/O)
  0     'infio ' = layer k   i.dep. I/O unit (0 no I/O)
  0     'thkio ' = layer k   thick. I/O unit (0 no I/O)
 33     'temio ' = layer k   temp   I/O unit (0 no I/O)
 34     'salio ' = layer k   saln.  I/O unit (0 no I/O)
 35     'tthio ' = layer k   dens,  I/O unit (0 no I/O)
  0     'sfnio ' = layer k  strmfn. I/O unit (0 no I/O)
  0	'kf    ' = first output layer (=0 end output; <0 label with layer #)
EOF

touch sshio.nc uvlio.nc vvlio.nc thkio.nc 
/bin/rm sshio.nc uvlio.nc vvlio.nc thkio.nc

setenv CDF023 sshio.nc
setenv CDF024 mltio.nc
setenv CDF054 uvlio.nc
setenv CDF055 vvlio.nc
setenv CDF033 temio.nc
setenv CDF034 salio.nc
setenv CDF035 tthio.nc
setenv DsyOyr 15
setenv CDF_INST 'NODC Levitus+historical data'
setenv CDF_TITLE 'North Atlantic (almost Levitus levels)'

touch regional.depth.b regional.depth.a
/bin/rm regional.depth.b regional.depth.a
ln -s $DEPTHFILEa regional.depth.a
ln -s $DEPTHFILEb regional.depth.b
${execnc}/archv2ncdf2d < archv2ncdf2d.in > archv2ncdf2d.log

# make the directories on the remote host
setenv data_remote /usr2/seaspara/simulations/test/2d/
ssh -l seaspara stommel.ncep.noaa.gov mkdir -p ${data_remote}

scp sshio.nc seaspara@stommel:$data_remote/sshio.nc
scp mltio.nc seaspara@stommel:$data_remote/mltio.nc
scp uvlio.nc seaspara@stommel:$data_remote/uvlio.nc
scp vvlio.nc seaspara@stommel:$data_remote/vvlio.nc
scp temio.nc seaspara@stommel:$data_remote/temio.nc
scp salio.nc seaspara@stommel:$data_remote/salio.nc
scp tthio.nc seaspara@stommel:$data_remote/tthio.nc

