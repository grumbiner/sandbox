#!/bin/ksh

# -------------------------------------------------------------
#    Overhead to ensure needed files are available
#    Will not be needed in operational version of script

set -x
# prepare archv23z
export execnc=/nfsuser/g01/wx21rg/abin/archiv_for_grib/exec
echo $execnc

export  INPUTDIR=/nfsuser/g01/seaspara/users/avichal/archiv_for_grib/inputs
export  WORKDIR=/gpfstmp/wx21rg/grib
export  ARCHDIR=/gpfs/b/ptmp/seas/hycom.para/forecast

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
if [ ! -f regional.depth.a ] ; then
  rcp $DEPTHFILEa .
fi
if [ ! -f regional.depth.b ] ; then
  rcp $DEPTHFILEb .
fi

if [ ! -f archv.b ] ; then
  cp $ARCHDIR/archv.2005_206_00.b  archv.b
fi
if [ ! -f archv.a ] ; then
  cp $ARCHDIR/archv.2005_206_00.a  archv.a
fi

# ----------------------
#Argument-able / environment-able variables:
#PDY is operations date variable, HH is hour/cycle of run:
PDY=20050725
HH=00

#LEAD is the number of hours lead in archv.a,.b file
LEAD=24

#D2 is a number, any nonzero value, for gribbing 2d fields.  0 to skip
#D3 is likewise for the 3d field
D2=25
D3=55

#    End of setting up/preparing the script.  Need no changes below here
#  for ops.
# ----------------------

missing=no
for fn in regional.depth.a regional.depth.b regional.grid.a regional.grid.b archv.a archv.b
do
  if [ ! -f $fn ] ; then
    missing=yes
    echo Missing file $fn, will not be able to run
  fi
done
if [ missing = 'yes' ] ; then
  echo Cannot run due to missing files.
  exit 1
fi 

touch archv2grib3z.in
/bin/rm archv2grib3z.in

YYYY=`echo $PDY | cut -c1-4`
MM=`echo $PDY | cut -c5-6`
DD=`echo $PDY | cut -c7-8`


cat >  archv2grib3z.in<<EOF
archv.b
GRIB
$YYYY $MM $DD $HH
$LEAD
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
$D2     'sshio ' = sea surface height 
$D2     'uvmio ' = mix layer u-vel 
$D2     'vvmio ' = mix layer v-vel 
$D2     'sstio ' = mix later temperature 
$D2     'sssio ' = mix later salinity 
$D2     'ssdio ' = mix later density 
$D2     'mtgio ' = Montgomery stream function
  0     'bsfio ' = barotropic stmfn 
  0	'botio ' = bathymetry       I/O unit (0 no I/O)
$D2	'mltio ' = mix. lay. thick. I/O unit (0 no I/O)
  0 	'infio ' = interface depths I/O unit (0 no I/O)
  0	'wviio ' = intf. w-velocity I/O unit (0 no I/O)
  0	'wvlio ' = w-velocity       I/O unit (0 no I/O)
$D3	'uvlio ' = u-velocity       I/O unit (0 no I/O)
$D3	'vvlio ' = v-velocity       I/O unit (0 no I/O)
  0	'splio ' = speed            I/O unit (0 no I/O)
$D3	'temio ' = temperature      I/O unit (0 no I/O)
$D3	'salio ' = salinity         I/O unit (0 no I/O)
$D3	'tthio ' = density          I/O unit (0 no I/O)
 51	'grbio ' = grib output file I/O unit (0 no I/O)
  0     'datsrc' = climatology type (-1=pom;0=hycom;1=igor,2=gdem,3=levitus)
  0	'keio  ' = kinetic energy   I/O unit (0 no I/O)
EOF

${execnc}/archvgrib < archv2grib3z.in > archvgrib.log

