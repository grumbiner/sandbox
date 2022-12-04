#!/bin/ksh

set -x
# prepare archv23z
export execnc=/nfsuser/g01/wx21rg/abin/archiv_for_grib/exec
echo $execnc
PDY=20050824

export  INPUTDIR=/nfsuser/g01/seaspara/users/avichal/archiv_for_grib/inputs
export  WORKDIR=/gpfstmp/wx21rg/grib
#export  ARCHDIR	    /gpfs/f/ptmp/seaspara/avichal/nowcast
#export  ARCHDIR    /gpfs/b/ptmp/seas/hycom.para/nowcast
#export  ARCHDIR=/gpfs/b/ptmp/seas/hycom.para/nowcast
export  ARCHDIR=/com/ofs/para/ofs.$PDY/

export DEPTHFILEa=${INPUTDIR}/regional.depth.a
export DEPTHFILEb=${INPUTDIR}/regional.depth.b

export GRIDFILEa=${INPUTDIR}/regional.grid.a
export GRIDFILEb=${INPUTDIR}/regional.grid.b
                                                                                                                    
echo WORKDIR  =   $WORKDIR
echo $DEPTHFILEa
echo $GRIDFILEa
mkdir -p $WORKDIR
cd $WORKDIR

if [ ! -f regional.grid.a ] ; then
  rcp $GRIDFILEa .
fi
if [ ! -f regional.grid.b ] ; then
  rcp $GRIDFILEb .
fi

if [ ! -f archv.b ] ; then
  cp $ARCHDIR/ofs_atl.t00z.2005_238_00.b  archv.b
fi
if [ ! -f archv.a ] ; then
  cp $ARCHDIR/ofs_atl.t00z.2005_238_00.a  archv.a
fi

touch archv2grib3z.in
/bin/rm archv2grib3z.in

cat >  archv2grib3z.in<<EOF
archv.b
GRIB
2005 08 24 00
24
000	'iexpt ' = experiment number x10 (000=from archive file)
  3	'yrflag' = days in year flag (0=360J16,1=366J16,2=366J01,3-actual)
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
 36     'kz    ' = number of depths to sample
  0     'z    '  = sample depth
  3     'z    '  = sample depth
  6     'z    '  = sample depth
 10     'z    '  = sample depth
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
400     'z    '  = sample depth
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
  0     'flxio ' = surface heat flux
  0     'empio ' = surf. evap-precip 
  0     'evpio ' = surface evaporative 
  0     'snsio ' = surface sensible head 
 23     'sshio ' = sea surface height 
 24     'uvmio ' = mix layer u-vel 
 25     'vvmio ' = mix layer v-vel 
 26     'sstio ' = mix later temperature 
 27     'sssio ' = mix later salinity 
 28     'ssdio ' = mix later density 
 23     'mtgio ' = Montgomery stream function
  0     'bsfio ' = barotropic stmfn 
  0	'botio ' = bathymetry       I/O unit (0 no I/O)
 62	'mltio ' = mix. lay. thick. I/O unit (0 no I/O)
  0 	'infio ' = interface depths I/O unit (0 no I/O)
  0	'wviio ' = intf. w-velocity I/O unit (0 no I/O)
  0	'wvlio ' = w-velocity       I/O unit (0 no I/O)
 66	'uvlio ' = u-velocity       I/O unit (0 no I/O)
 67	'vvlio ' = v-velocity       I/O unit (0 no I/O)
  0	'splio ' = speed            I/O unit (0 no I/O)
 69	'temio ' = temperature      I/O unit (0 no I/O)
 70	'salio ' = salinity         I/O unit (0 no I/O)
 71	'tthio ' = density          I/O unit (0 no I/O)
 51	'grbio ' = grib output file I/O unit (0 no I/O)
  0     'datsrc' = climatology type (-1=pom;0=hycom;1=igor,2=gdem,3=levitus)
  0	'keio  ' = kinetic energy   I/O unit (0 no I/O)
EOF

if [ ! -f regional.depth.a ] ; then
  rcp $DEPTHFILEa .
fi
if [ ! -f regional.depth.b ] ; then
  rcp $DEPTHFILEb .
fi
#${execnc}/archv2ncdf3z < archv2grib3z.in > archv2ncdf3z.log
${execnc}/archvgrib < archv2grib3z.in > archvgrib.log

wgrib -V fort.51 > out

#RG notes:
#gribio is for grib output, specifying an output file unit
#  If grib output is not being done, this is ignored
