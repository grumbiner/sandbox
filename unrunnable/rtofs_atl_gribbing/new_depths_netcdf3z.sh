#!/bin/csh

# prepare archv23z
setenv execnc /nfsuser/g01/seaspara/users/avichal/archiv_for_grib/exec
echo $execnc

setenv  INPUTDIR    /nfsuser/g01/seaspara/users/avichal/archiv_for_grib/inputs
setenv  WORKDIR     /gpfstmp/seaspara/netcdf
setenv  ARCHDIR	    /gpfs/f/ptmp/seaspara/avichal/nowcast

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

touch archv2ncdf3z.in
/bin/rm archv2ncdf3z.in

cat >  archv2ncdf3z.in<<EOF
archv.b
NetCDF
000	'iexpt ' = experiment number x10 (000=from archive file)
  0	'yrflag' = days in year flag (0=360J16,1=366J16,2=366J01,3-actual)
 1200	'idm   ' = longitudinal array size
 1684	'jdm   ' = latitudinal  array size
 25	'kdm   ' = number of layers
 34.0	'thbase' = reference density (sigma units)
  0	'smooth' = smooth fields before plotting (0=F,1=T)
  1	'iorign' = i-origin of plotted subregion
  1	'jorign' = j-origin of plotted subregion
  0     'idmp  ' = i-extent of plotted subregion (<=idm; 0 implies idm)
  0     'jdmp  ' = j-extent of plotted subregion (<=jdm; 0 implies jdm)
  1     'itype ' = interpolation type (0=sample,1=linear)
 40     'kz    ' = number of depths to sample
  0     'z    '  = sample depth
  3     'z    '  = sample depth
  6     'z    '  = sample depth
  9     'z    '  = sample depth
 12     'z    '  = sample depth
 15     'z    '  = sample depth
 20     'z    '  = sample depth
 30     'z    '  = sample depth
 50     'z    '  = sample depth
 75     'z    '  = sample depth
100     'z    '  = sample depth
125     'z    '  = sample depth
150     'z    '  = sample depth
200     'z    '  = sample depth
250     'z    '  = sample depth
300     'z    '  = sample depth
350     'z    '  = sample depth
400     'z    '  = sample depth
450     'z    '  = sample depth
500     'z    '  = sample depth
600     'z    '  = sample depth
700     'z    '  = sample depth
800     'z    '  = sample depth
900     'z    '  = sample depth
1000    'z    '  = sample depth
1100    'z    '  = sample depth
1200    'z    '  = sample depth
1300    'z    '  = sample depth
1400    'z    '  = sample depth
1500    'z    '  = sample depth
1750    'z    '  = sample depth
2000    'z    '  = sample depth
2500    'z    '  = sample depth
3000    'z    '  = sample depth
3500    'z    '  = sample depth
4000    'z    '  = sample depth
4500    'z    '  = sample depth
5000    'z    '  = sample depth
5500    'z    '  = sample depth
6000    'z    '  = sample depth
  0	'botio ' = bathymetry       I/O unit (0 no I/O)
  0	'mltio ' = mix. lay. thick. I/O unit (0 no I/O)
 63 	'infio ' = interface depths I/O unit (0 no I/O)
  0	'wviio ' = intf. w-velocity I/O unit (0 no I/O)
 65	'wvlio ' = w-velocity       I/O unit (0 no I/O)
 66	'uvlio ' = u-velocity       I/O unit (0 no I/O)
 67	'vvlio ' = v-velocity       I/O unit (0 no I/O)
  0	'splio ' = speed            I/O unit (0 no I/O)
 69	'temio ' = temperature      I/O unit (0 no I/O)
 70	'salio ' = salinity         I/O unit (0 no I/O)
 71	'tthio ' = density          I/O unit (0 no I/O)
  0     'datsrc' = climatology type (-1=pom;0=hycom;1=igor,2=gdem,3=levitus)
  0	'keio  ' = kinetic energy   I/O unit (0 no I/O)
EOF

touch infio.nc wvlio.nc temio.nc salio.nc tthio.nc uvlio.nc vvlio.nc
/bin/rm infio.nc wvlio.nc temio.nc salio.nc tthio.nc uvlio.nc vvlio.nc

setenv CDF063 infio.nc
setenv CDF065 wvlio.nc
setenv CDF066 uvlio.nc
setenv CDF067 vvlio.nc
setenv CDF069 temio.nc
setenv CDF070 salio.nc
setenv CDF071 tthio.nc
setenv DsyOyr  15
setenv CDF_INST  'NODC Levitus+historical data'
setenv CDF_TITLE  'North Atlantic (almost Levitus levels)'

touch regional.depth.b regional.depth.a
/bin/rm regional.depth.b regional.depth.a
ln -s $DEPTHFILEa regional.depth.a
ln -s $DEPTHFILEb regional.depth.b
${execnc}/archv2ncdf3z < archv2ncdf3z.in > archv2ncdf3z.log

# make the directories on the remote host
setenv data_remote /usr2/seaspara/simulations/test/
ssh -l seaspara stommel.ncep.noaa.gov mkdir -p ${data_remote}

scp infio.nc seaspara@stommel:$data_remote/infio.nc
scp temio.nc seaspara@stommel:$data_remote/temio.nc
scp salio.nc seaspara@stommel:$data_remote/salio.nc
scp tthio.nc seaspara@stommel:$data_remote/tthio.nc
scp uvlio.nc seaspara@stommel:$data_remote/uvlio.nc
scp vvlio.nc seaspara@stommel:$data_remote/vvlio.nc
scp wvlio.nc seaspara@stommel:$data_remote/wvlio.nc

