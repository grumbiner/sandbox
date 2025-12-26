#! /bin/sh

# @ step_name = rcdas_ingest
# @ output = job.$(jobid).$(stepid).$(step_name).out
# @ error = job.$(jobid).$(stepid).$(step_name).err
# @ job_type = serial
#
#### @ node = 1
#### @ node_usage = not_shared
#
#
#### @ wall_clock_limit = 00:00:00
# @ class = dev
# @ group = devonprod
# @ queue

set -aeux

export PS4='$SECONDS + '
echo "$0 STRDATE "`date`

echo ${NARR:?"NARR is not set; fatal error"}

export STREAM=`cut -c 1-2 STREAM`
readonly STREAM
echo ${STREAM:?"STREAM is not set; fatal error"}

. ${NARR}/scr/narr_envir.sh

INIDIR=`pwd`
readonly INIDIR

DATE=`cut -c 1-10 ${INIDIR}/INGESTDATE`

#DATE=2004040600

mkdir -p /ptmp/wx52rr/rcdas
cd /ptmp/wx52rr/rcdas
rm -f /ptmp/wx52rr/rcdas/*
rm -f -r /ptmp/wx52rr/rcdas_out/*

export DATE
echo $DATE > curdate
CDATE=`cut -c 1-8 curdate`
export CDATE
YEAR4=`cut -c 1-4 curdate`
export YEAR4
YEAR2=`cut -c 3-4 curdate`
export YEAR2
MONTH=`cut -c 5-6 curdate`
export MONTH
DAY=`cut -c 7-8 curdate`
export DAY

DATE06=`/nwprod/util/exec/ndate +6 $DATE`
DATE12=`/nwprod/util/exec/ndate +12 $DATE`
DATE18=`/nwprod/util/exec/ndate +18 $DATE`
export DATM1=`/nwprod/util/exec/ndate -24 $DATE`
export DATE24=`/nwprod/util/exec/ndate +24 $DATE`
export DATE06 DATE12 DATE18

echo $DATM1 > curdate
CDATM1=`cut -c 1-8 curdate`
export CDATM1

echo $DATE24 > curdate
CDATP1=`cut -c 1-8 curdate`
export CDATP1

export DIREDAS=/com/eta/prod/edas.${CDATE}
export DIREDASM1=/com/eta/prod/edas.${CDATM1}
export DIREDASP1=/com/eta/prod/edas.${CDATP1}
export DIRSST=/com/fnl/prod/sst.${CDATE}
#export DIRCDAS=/com/cdas/prod/cdas.${CDATE}
export DIRCDAS=/nfsuser/g03/wx51we/data/monthdir
export DIRNLDAS=/ptmp/wx22dl/LDAS/${CDATE}
export DIRNLDASP1=/ptmp/wx22dl/LDAS/${CDATP1}
export OUTRCDAS=/ptmp/wx52rr/rcdas.${CDATE}
export TOVS=${NARR}/bufr2ieee
mkdir -p $OUTRCDAS

### Ingest data

cp $DIREDASP1/edas.t18z.snowdepth.grb snowdepth.$DATE

if [ -e $DIREDAS/edas.t18z.imssnow.grb -a -s $DIREDAS/edas.t18z.imssnow.grb ]; then
   cp $DIREDAS/edas.t18z.imssnow.grb ice.$DATE

else 
#   htar -xvf /hpssprod/runhistory/rh${YEAR4}/${YEAR4}${MONTH}/${YEAR4}${MONTH}${DAY}/com_eta_prod_edas.${YEAR4}${MONTH}${DAY}18.tar ./edas.t18z.imssnow.grb

   htar -xvf /hpssprod/runhistory/rh${YEAR4}/${YEAR4}${MONTH}/${YEAR4}${MONTH}${DAY}/com_nam_prod_ndas.${YEAR4}${MONTH}${DAY}18.tar ./ndas.t18z.imssnow.grb

   mv ./ndas.t18z.imssnow.grb ice.$DATE
fi

cp $DIRSST/sst.t12z.eta_grid glsst.$DATE
cp $DIRCDAS/sstgrb${DATE12} sst.$DATE
cp $DIRCDAS/1bhrs2*${CDATE}* .
cp $DIRCDAS/1bhrs3*${CDATE}* .
cp $DIRCDAS/1bmsu*${CDATE}* .
cp $DIRCDAS/1bhrs2*${DATE24}* .
cp $DIRCDAS/1bhrs3*${DATE24}* .
cp $DIRCDAS/1bmsu*${DATE24}* .
cp $DIRCDAS/sanl*${CDATE}* .
cp $DIRCDAS/sanl${DATE24} .
cp $DIRCDAS/sfcanl*${CDATE}* .
cp $DIRCDAS/grb2d*${CDATE}* .
cp $DIRCDAS/prepqm${CDATE}*post* .
cp $DIRCDAS/prepqm${DATE24}*post* .
cp $DIRNLDAS/${CDATE}*noaa .
cp $DIRNLDASP1/${DATE24}*noaa .

# get data from workstations
ftp XXXX << EOF > ftp.out
binary
prompt off
cd /xxx/GRIB_US_MEX_FILES
pwd
mget ${CDATE}*.us_mex.grb
cd /xxx/GRIB_CMORPH_FILES
pwd
mget CMORPH_025deg_${CDATE}*.hourly.grb
bye
EOF

### End ingest

### Precipitation processing

DATPCP=$DATE
DATEND=$DATE24

while [ $DATPCP -le $DATEND ]; do

echo $DATPCP > curdate
CDATE=`cut -c 3-10 curdate`
DATPCPM1=`/nwprod/util/exec/ndate -1 $DATPCP`

wgrib -d 15 ${DATPCP}.lsmforce_noaa | wgrib -i ${DATPCP}.lsmforce_noaa -grib -o ${DATPCP}.lsmforce_apcp15
copygb -g192 -i6 -x ${DATPCP}.lsmforce_apcp15 phourly_ldas.${DATPCPM1}

if [ $DATPCP -ne $DATEND ]; then
   copygb -g192 -i0 -x ${DATPCP}.us_mex.grb mexico.${DATPCP}
   copygb -g192 -i6 -x CMORPH_025deg_${DATPCP}.hourly.grb cmorph.${DATPCP}
fi

DATPCP=`/nwprod/util/exec/ndate +1 $DATPCP`

done

DATP1=`/nwprod/util/exec/ndate +1 $DATE`
DATP2=`/nwprod/util/exec/ndate +2 $DATE`
DATP3=`/nwprod/util/exec/ndate +3 $DATE`
DATP4=`/nwprod/util/exec/ndate +4 $DATE`
DATP5=`/nwprod/util/exec/ndate +5 $DATE`
DATP6=`/nwprod/util/exec/ndate +6 $DATE`
DATP7=`/nwprod/util/exec/ndate +7 $DATE`
DATP8=`/nwprod/util/exec/ndate +8 $DATE`
DATP9=`/nwprod/util/exec/ndate +9 $DATE`
DATP10=`/nwprod/util/exec/ndate +10 $DATE`
DATP11=`/nwprod/util/exec/ndate +11 $DATE`
DATP12=`/nwprod/util/exec/ndate +12 $DATE`
DATP13=`/nwprod/util/exec/ndate +13 $DATE`
DATP14=`/nwprod/util/exec/ndate +14 $DATE`
DATP15=`/nwprod/util/exec/ndate +15 $DATE`
DATP16=`/nwprod/util/exec/ndate +16 $DATE`
DATP17=`/nwprod/util/exec/ndate +17 $DATE`
DATP18=`/nwprod/util/exec/ndate +18 $DATE`
DATP19=`/nwprod/util/exec/ndate +19 $DATE`
DATP20=`/nwprod/util/exec/ndate +20 $DATE`
DATP21=`/nwprod/util/exec/ndate +21 $DATE`
DATP22=`/nwprod/util/exec/ndate +22 $DATE`
DATP23=`/nwprod/util/exec/ndate +23 $DATE`
DATP24=`/nwprod/util/exec/ndate +24 $DATE`

ln -s -f phourly_ldas.$DATE fort.10
ln -s -f phourly_ldas.$DATP1 fort.11
ln -s -f phourly_ldas.$DATP2 fort.12
ln -s -f phourly_ldas.$DATP3 fort.13
ln -s -f phourly_ldas.$DATP4 fort.14
ln -s -f phourly_ldas.$DATP5 fort.15
ln -s -f phourly_ldas.$DATP6 fort.16
ln -s -f phourly_ldas.$DATP7 fort.17
ln -s -f phourly_ldas.$DATP8 fort.18
ln -s -f phourly_ldas.$DATP9 fort.19
ln -s -f phourly_ldas.$DATP10 fort.20
ln -s -f phourly_ldas.$DATP11 fort.21
ln -s -f phourly_ldas.$DATP12 fort.22
ln -s -f phourly_ldas.$DATP13 fort.23
ln -s -f phourly_ldas.$DATP14 fort.24
ln -s -f phourly_ldas.$DATP15 fort.25
ln -s -f phourly_ldas.$DATP16 fort.26
ln -s -f phourly_ldas.$DATP17 fort.27
ln -s -f phourly_ldas.$DATP18 fort.28
ln -s -f phourly_ldas.$DATP19 fort.29
ln -s -f phourly_ldas.$DATP20 fort.30
ln -s -f phourly_ldas.$DATP21 fort.31
ln -s -f phourly_ldas.$DATP22 fort.32
ln -s -f phourly_ldas.$DATP23 fort.33
ln -s -f cmorph.$DATE  fort.40
ln -s -f cmorph.$DATP1 fort.41
ln -s -f cmorph.$DATP2 fort.42
ln -s -f cmorph.$DATP3 fort.43
ln -s -f cmorph.$DATP4 fort.44
ln -s -f cmorph.$DATP5 fort.45
ln -s -f cmorph.$DATP6 fort.46
ln -s -f cmorph.$DATP7 fort.47
ln -s -f cmorph.$DATP8 fort.48
ln -s -f cmorph.$DATP9 fort.49
ln -s -f cmorph.$DATP10 fort.50
ln -s -f cmorph.$DATP11 fort.51
ln -s -f cmorph.$DATP12 fort.52
ln -s -f cmorph.$DATP13 fort.53
ln -s -f cmorph.$DATP14 fort.54
ln -s -f cmorph.$DATP15 fort.55
ln -s -f cmorph.$DATP16 fort.56
ln -s -f cmorph.$DATP17 fort.57
ln -s -f cmorph.$DATP18 fort.58
ln -s -f cmorph.$DATP19 fort.59
ln -s -f cmorph.$DATP20 fort.60
ln -s -f cmorph.$DATP21 fort.61
ln -s -f cmorph.$DATP22 fort.62
ln -s -f cmorph.$DATP23 fort.63
ln -s -f mexico.$DATE  fort.70
ln -s -f mexico.$DATP1 fort.71
ln -s -f mexico.$DATP2 fort.72
ln -s -f mexico.$DATP3 fort.73
ln -s -f mexico.$DATP4 fort.74
ln -s -f mexico.$DATP5 fort.75
ln -s -f mexico.$DATP6 fort.76
ln -s -f mexico.$DATP7 fort.77
ln -s -f mexico.$DATP8 fort.78
ln -s -f mexico.$DATP9 fort.79
ln -s -f mexico.$DATP10 fort.80
ln -s -f mexico.$DATP11 fort.81
ln -s -f mexico.$DATP12 fort.82
ln -s -f mexico.$DATP13 fort.83
ln -s -f mexico.$DATP14 fort.84
ln -s -f mexico.$DATP15 fort.85
ln -s -f mexico.$DATP16 fort.86
ln -s -f mexico.$DATP17 fort.87
ln -s -f mexico.$DATP18 fort.88
ln -s -f mexico.$DATP19 fort.89
ln -s -f mexico.$DATP20 fort.90
ln -s -f mexico.$DATP21 fort.91
ln -s -f mexico.$DATP22 fort.92
ln -s -f mexico.$DATP23 fort.93
ln -s -f ${NARR}/precip_scripts/2degmaskout.grb_new fort.94
ln -s -f ${NARR}/precip_scripts/EGRD3D00.tm12 fort.96
ln -s -f ${NARR}/precip_scripts/hgtsmref.r3245.2d_tmasks fort.95
ln -s -f egrd32.0to2      fort.101
ln -s -f egrd32.3to5      fort.102
ln -s -f egrd32.6to8      fort.103
ln -s -f egrd32.9to11     fort.104
ln -s -f egrd32.12to14    fort.105
ln -s -f egrd32.15to17    fort.106
ln -s -f egrd32.18to20    fort.107
ln -s -f egrd32.21to23    fort.108

echo $YEAR2 $MONTH $DAY| ${NARR}/precip_scripts/pcpto32.x > pcp.print
export err=$?

if [ $err -ne 0 ]; then
  echo "*********************************"
  echo "*  THERE IS SOME MISSING PRECIP *"
  echo "*********************************"
fi

mkdir -p $OUTRCDAS/$DATP12
mkdir -p $OUTRCDAS/$DATP24
cp egrd32.0to2 $OUTRCDAS/$DATP12/pcp32.tm12.3hr
cp egrd32.3to5 $OUTRCDAS/$DATP12/pcp32.tm09.3hr
cp egrd32.6to8 $OUTRCDAS/$DATP12/pcp32.tm06.3hr
cp egrd32.9to11 $OUTRCDAS/$DATP12/pcp32.tm03.3hr
cp egrd32.12to14 $OUTRCDAS/$DATP24/pcp32.tm12.3hr
cp egrd32.15to17 $OUTRCDAS/$DATP24/pcp32.tm09.3hr
cp egrd32.18to20 $OUTRCDAS/$DATP24/pcp32.tm06.3hr
cp egrd32.21to23 $OUTRCDAS/$DATP24/pcp32.tm03.3hr

export PCPDATA8=${INPUTNARR}/pcpdata_prod

mkdir -p $PCPDATA8/$DATP12
mkdir -p $PCPDATA8/$DATP24

cp egrd32.0to2 $PCPDATA8/$DATP12/egrd80.tm12.3hr
cp egrd32.3to5 $PCPDATA8/$DATP12/egrd80.tm09.3hr
cp egrd32.6to8 $PCPDATA8/$DATP12/egrd80.tm06.3hr
cp egrd32.9to11 $PCPDATA8/$DATP12/egrd80.tm03.3hr
cp egrd32.12to14 $PCPDATA8/$DATP24/egrd80.tm12.3hr
cp egrd32.15to17 $PCPDATA8/$DATP24/egrd80.tm09.3hr
cp egrd32.18to20 $PCPDATA8/$DATP24/egrd80.tm06.3hr
cp egrd32.21to23 $PCPDATA8/$DATP24/egrd80.tm03.3hr

### End precip processing

export SST32=${INPUTNARR}/sst32
export SNOW32=${INPUTNARR}/snow32
export ICE32=${INPUTNARR}/ice32

### Snow processing

ln -s -f snowdepth.$DATE           fort.41
ln -s -f ${FIX}/rfusaflw            fort.42
ln -s -f ${NARR}/precip_scripts/EGRD3D00.tm12     fort.43
ln -s -f ${NARR}/precip_scripts/hgtsmref.r3245.2d_tmasks fort.44
ln -s -f ${INPUTNARR}/nhb/nhb3245      fort.46
ln -s -f $OUTRCDAS/snowdepth.${DATE}.grb    fort.50

echo $YEAR2 $MONTH $DAY| ${NARR}/snowblend/snowget.x > snowprint
export err=$?

if [ $err -ne 0 ]; then
  echo "*********************************"
  echo "*  THERE IS NO SNOW             *"
  echo "*********************************"
fi

cat $OUTRCDAS/snowdepth.${DATE}.grb >> $SNOW32/snowd.nh.$YEAR4

### End snow processing

### Ice processing

copygb -g192 -i0 -x ice.$DATE ice2.$DATE

ln -s -f ${NARR}/precip_scripts/hgtsmref.r3245.2d_tmasks fort.11
ln -s -f ${NARR}/iceclim/ice_canada.tbl                  fort.12
ln -s -f ${NARR}/iceclim/ice.clim.grb                    fort.20
ln -s -f ice2.$DATE                                                  fort.30
ln -s -f $OUTRCDAS/ice.${DATE}.grb                      fort.50

echo $YEAR2 $MONTH $DAY| ${NARR}/iceclim/iceblend.x > iceprint

cat $OUTRCDAS/ice.${DATE}.grb >> $ICE32/ice.$YEAR4.grb
export err=$?

if [ $err -ne 0 ]; then
  echo "*********************************"
  echo "*  THERE IS NO ICE              *"
  echo "*********************************"
fi

### End ice processing

### SST processing

ln -s -f ${NARR}/iceclim/ice_canada.tbl    fort.10
ln -s -f ${NARR}/precip_scripts/hgtsmref.r3245.2d_tmasks fort.14
ln -s -f sst.$DATE            fort.39
ln -s -f $OUTRCDAS/ice.${DATE}.grb            fort.40
ln -s -f glsst.$DATE                                       fort.41
ln -s -f $OUTRCDAS/sst.${DATE}.grb            fort.50

echo $YEAR2 $MONTH $DAY| ${NARR}/sstblend/getsst.x > sstprint
export err=$?

if [ $err -ne 0 ]; then
  echo "*********************************"
  echo "*  THERE IS NO SST              *"
  echo "*********************************"
fi


cat $OUTRCDAS/sst.${DATE}.grb >> $SST32/sst.$YEAR4.grb

### End SST processing

### TOVS H1B processing

DATE03=`/nwprod/util/exec/ndate +3 $DATE`
DATE06=`/nwprod/util/exec/ndate +6 $DATE`
DATE09=`/nwprod/util/exec/ndate +9 $DATE`
DATE12=`/nwprod/util/exec/ndate +12 $DATE`
DATE15=`/nwprod/util/exec/ndate +15 $DATE`
DATE18=`/nwprod/util/exec/ndate +18 $DATE`
DATE21=`/nwprod/util/exec/ndate +21 $DATE`
DATE24=`/nwprod/util/exec/ndate +24 $DATE`
export DATE03 DATE06 DATE09 DATE12 DATE15 DATE18 DATE21 DATE24

sh $TOVS/ieeetovs.sh 1bhrs2 14 $DATE
sh $TOVS/ieeetovs.sh 1bhrs3 15 $DATE
sh $TOVS/ieeetovs.sh 1bhrs3 16 $DATE
sh $TOVS/ieeetovs.sh 1bmsu 14 $DATE

sh $TOVS/ieeetovs.sh 1bhrs2 14 $DATE06
sh $TOVS/ieeetovs.sh 1bhrs3 15 $DATE06
sh $TOVS/ieeetovs.sh 1bhrs3 16 $DATE06
sh $TOVS/ieeetovs.sh 1bmsu 14 $DATE06

sh $TOVS/ieeetovs.sh 1bhrs2 14 $DATE12
sh $TOVS/ieeetovs.sh 1bhrs3 15 $DATE12
sh $TOVS/ieeetovs.sh 1bhrs3 16 $DATE12
sh $TOVS/ieeetovs.sh 1bmsu 14 $DATE12

sh $TOVS/ieeetovs.sh 1bhrs2 14 $DATE18
sh $TOVS/ieeetovs.sh 1bhrs3 15 $DATE18
sh $TOVS/ieeetovs.sh 1bhrs3 16 $DATE18
sh $TOVS/ieeetovs.sh 1bmsu 14 $DATE18

sh $TOVS/ieeetovs.sh 1bhrs2 14 $DATE24
sh $TOVS/ieeetovs.sh 1bhrs3 15 $DATE24
sh $TOVS/ieeetovs.sh 1bhrs3 16 $DATE24
sh $TOVS/ieeetovs.sh 1bmsu 14 $DATE24

sat="n14 n15 n16"

for s in $sat
do

export DIR1B=${INPUTNARR}/tovs_1b_data/$YEAR4/$MONTH
mkdir -p $DIR1B

ln -s -f $OUTRCDAS/h1b${s}.${DATE} fort.51
cat << EOF > input
h1b${s}.${DATE}.ieee_d h1b${s}.${DATE06}.ieee_d $DATE 3
EOF
${NARR}/gettovs/window1b_h1b.x < input >> h1bprint
if [ -s $OUTRCDAS/h1b${s}.${DATE} ]; then
cp $OUTRCDAS/h1b${s}.${DATE} $DIR1B
else
echo "****************************************"
echo "$OUTRCDAS/h1b${s}.${DATE} does not exist"
echo "****************************************"
fi

ln -s -f $OUTRCDAS/h1b${s}.${DATE03} fort.51
cat << EOF > input
h1b${s}.${DATE}.ieee_d h1b${s}.${DATE06}.ieee_d $DATE03 3
EOF
${NARR}/gettovs/window1b_h1b.x < input >> h1bprint
if [ -s $OUTRCDAS/h1b${s}.${DATE03} ]; then
cp $OUTRCDAS/h1b${s}.${DATE03} $DIR1B
else
echo "******************************************"
echo "$OUTRCDAS/h1b${s}.${DATE03} does not exist"
echo "******************************************"
fi

ln -s -f $OUTRCDAS/h1b${s}.${DATE06} fort.51
cat << EOF > input
h1b${s}.${DATE06}.ieee_d h1b${s}.${DATE12}.ieee_d $DATE06 3
EOF
${NARR}/gettovs/window1b_h1b.x < input >> h1bprint
if [ -s $OUTRCDAS/h1b${s}.${DATE06} ]; then
cp $OUTRCDAS/h1b${s}.${DATE06} $DIR1B
else
echo "******************************************"
echo "$OUTRCDAS/h1b${s}.${DATE06} does not exist"
echo "******************************************"
fi

ln -s -f $OUTRCDAS/h1b${s}.${DATE09} fort.51
cat << EOF > input
h1b${s}.${DATE06}.ieee_d h1b${s}.${DATE12}.ieee_d $DATE09 3
EOF
${NARR}/gettovs/window1b_h1b.x < input >> h1bprint
if [ -s $OUTRCDAS/h1b${s}.${DATE09} ]; then
cp $OUTRCDAS/h1b${s}.${DATE09} $DIR1B
else
echo "******************************************"
echo "$OUTRCDAS/h1b${s}.${DATE09} does not exist"
echo "******************************************"
fi

ln -s -f $OUTRCDAS/h1b${s}.${DATE12} fort.51
cat << EOF > input
h1b${s}.${DATE06}.ieee_d h1b${s}.${DATE12}.ieee_d $DATE12 3
EOF
${NARR}/gettovs/window1b_h1b.x < input >> h1bprint
if [ -s $OUTRCDAS/h1b${s}.${DATE12} ]; then
cp $OUTRCDAS/h1b${s}.${DATE12} $DIR1B
else
echo "******************************************"
echo "$OUTRCDAS/h1b${s}.${DATE12} does not exist"
echo "******************************************"
fi

ln -s -f $OUTRCDAS/h1b${s}.${DATE15} fort.51
cat << EOF > input
h1b${s}.${DATE12}.ieee_d h1b${s}.${DATE18}.ieee_d $DATE15 3
EOF
${NARR}/gettovs/window1b_h1b.x < input >> h1bprint
if [ -s $OUTRCDAS/h1b${s}.${DATE15} ]; then
cp $OUTRCDAS/h1b${s}.${DATE15} $DIR1B
else
echo "******************************************"
echo "$OUTRCDAS/h1b${s}.${DATE15} does not exist"
echo "******************************************"
fi

ln -s -f $OUTRCDAS/h1b${s}.${DATE18} fort.51
cat << EOF > input
h1b${s}.${DATE18}.ieee_d h1b${s}.${DATE24}.ieee_d $DATE18 3
EOF
${NARR}/gettovs/window1b_h1b.x < input >> h1bprint
if [ -s $OUTRCDAS/h1b${s}.${DATE18} ]; then
cp $OUTRCDAS/h1b${s}.${DATE18} $DIR1B
else
echo "******************************************"
echo "$OUTRCDAS/h1b${s}.${DATE18} does not exist"
echo "******************************************"
fi

ln -s -f $OUTRCDAS/h1b${s}.${DATE21} fort.51
cat << EOF > input
h1b${s}.${DATE18}.ieee_d h1b${s}.${DATE24}.ieee_d $DATE21 3
EOF
${NARR}/gettovs/window1b_h1b.x < input >> h1bprint
if [ -s $OUTRCDAS/h1b${s}.${DATE21} ]; then
cp $OUTRCDAS/h1b${s}.${DATE21} $DIR1B
else
echo "******************************************"
echo "$OUTRCDAS/h1b${s}.${DATE21} does not exist"
echo "******************************************"
fi

done

### End TOVS H1B processing

### TOVS M1B processing

sat="n14"

for s in $sat
do

ln -s -f $OUTRCDAS/m1b${s}.${DATE} fort.51
cat << EOF > input
m1b${s}.${DATE}.ieee_d m1b${s}.${DATE06}.ieee_d $DATE 3
EOF
${NARR}/gettovs/window1b_m1b.x < input >> m1bprint
if [ -s $OUTRCDAS/m1b${s}.${DATE} ]; then
cp $OUTRCDAS/m1b${s}.${DATE} $DIR1B
else
echo "******************************************"
echo "$OUTRCDAS/m1b${s}.${DATE} does not exist"
echo "******************************************"
fi

ln -s -f $OUTRCDAS/m1b${s}.${DATE03} fort.51
cat << EOF > input
m1b${s}.${DATE}.ieee_d m1b${s}.${DATE06}.ieee_d $DATE03 3
EOF
${NARR}/gettovs/window1b_m1b.x < input >> m1bprint
if [ -s $OUTRCDAS/m1b${s}.${DATE03} ]; then
cp $OUTRCDAS/m1b${s}.${DATE03} $DIR1B
else
echo "******************************************"
echo "$OUTRCDAS/m1b${s}.${DATE03} does not exist"
echo "******************************************"
fi

ln -s -f $OUTRCDAS/m1b${s}.${DATE06} fort.51
cat << EOF > input
m1b${s}.${DATE06}.ieee_d m1b${s}.${DATE12}.ieee_d $DATE06 3
EOF
${NARR}/gettovs/window1b_m1b.x < input >> m1bprint
if [ -s $OUTRCDAS/m1b${s}.${DATE06} ]; then
cp $OUTRCDAS/m1b${s}.${DATE06} $DIR1B
else
echo "******************************************"
echo "$OUTRCDAS/m1b${s}.${DATE06} does not exist"
echo "******************************************"
fi

ln -s -f $OUTRCDAS/m1b${s}.${DATE09} fort.51
cat << EOF > input
m1b${s}.${DATE06}.ieee_d m1b${s}.${DATE12}.ieee_d $DATE09 3
EOF
${NARR}/gettovs/window1b_m1b.x < input >> m1bprint
if [ -s $OUTRCDAS/m1b${s}.${DATE09} ]; then
cp $OUTRCDAS/m1b${s}.${DATE09} $DIR1B
else
echo "******************************************"
echo "$OUTRCDAS/m1b${s}.${DATE09} does not exist"
echo "******************************************"
fi

ln -s -f $OUTRCDAS/m1b${s}.${DATE12} fort.51
cat << EOF > input
m1b${s}.${DATE06}.ieee_d m1b${s}.${DATE12}.ieee_d $DATE12 3
EOF
${NARR}/gettovs/window1b_m1b.x < input >> m1bprint
if [ -s $OUTRCDAS/m1b${s}.${DATE12} ]; then
cp $OUTRCDAS/m1b${s}.${DATE12} $DIR1B
else
echo "******************************************"
echo "$OUTRCDAS/m1b${s}.${DATE12} does not exist"
echo "******************************************"
fi

ln -s -f $OUTRCDAS/m1b${s}.${DATE15} fort.51
cat << EOF > input
m1b${s}.${DATE12}.ieee_d m1b${s}.${DATE18}.ieee_d $DATE15 3
EOF
${NARR}/gettovs/window1b_m1b.x < input >> m1bprint
if [ -s $OUTRCDAS/m1b${s}.${DATE15} ]; then
cp $OUTRCDAS/m1b${s}.${DATE15} $DIR1B
else
echo "******************************************"
echo "$OUTRCDAS/m1b${s}.${DATE15} does not exist"
echo "******************************************"
fi

ln -s -f $OUTRCDAS/m1b${s}.${DATE18} fort.51
cat << EOF > input
m1b${s}.${DATE18}.ieee_d m1b${s}.${DATE24}.ieee_d $DATE18 3
EOF
${NARR}/gettovs/window1b_m1b.x < input >> m1bprint
if [ -s $OUTRCDAS/m1b${s}.${DATE18} ]; then
cp $OUTRCDAS/m1b${s}.${DATE18} $DIR1B
else
echo "******************************************"
echo "$OUTRCDAS/m1b${s}.${DATE18} does not exist"
echo "******************************************"
fi

ln -s -f $OUTRCDAS/m1b${s}.${DATE21} fort.51
cat << EOF > input
m1b${s}.${DATE18}.ieee_d m1b${s}.${DATE24}.ieee_d $DATE21 3
EOF
${NARR}/gettovs/window1b_m1b.x < input >> m1bprint
if [ -s $OUTRCDAS/m1b${s}.${DATE21} ]; then
cp $OUTRCDAS/m1b${s}.${DATE21} $DIR1B
else
echo "******************************************"
echo "$OUTRCDAS/m1b${s}.${DATE21} does not exist"
echo "******************************************"
fi

done

### End TOVS M1B processing

### Boundary condition processing

ln -s -f sanl$DATE   fort.11
ln -s -f sanl$DATE06 fort.12
ln -s -f sanl$DATE12 fort.13
ln -s -f ${NARR}/mkbnd_sorc/modtop.parm  fort.41
ln -s -f ${NARR}/mkbnd_sorc/deta_ldt1.45.25mb fort.42
ln -s -f rvetalbc    fort.51
ln -s -f $OUTRCDAS/etabcs00_$DATE12  fort.52

${NARR}/mkbnd_sorc/eta_mkbnd < ${NARR}/mkbnd_sorc/mkbnd.parm_3 > mkbnd1print

export err=$?

if [ $err -ne 0 ]; then
  echo "*********************************"
  echo "* THERE ARE MISSING $DATE12 bcs *"
  echo "*********************************"
fi

ln -s -f sanl$DATE12 fort.11
ln -s -f sanl$DATE18 fort.12
ln -s -f sanl$DATE24  fort.13
ln -s -f ${NARR}/mkbnd_sorc/modtop.parm  fort.41
ln -s -f ${NARR}/mkbnd_sorc/deta_ldt1.45.25mb fort.42
ln -s -f rvetalbc    fort.51
ln -s -f $OUTRCDAS/etabcs00_$DATE24  fort.52

${NARR}/mkbnd_sorc/eta_mkbnd < ${NARR}/mkbnd_sorc/mkbnd.parm_3 > mkbnd2print
export err=$?

if [ $err -ne 0 ]; then
  echo "*********************************"
  echo "* THERE ARE MISSING $DATE24 bcs *"
  echo "*********************************"
fi

export ETABCSDIR=${INPUTNARR}/etabcs/$YEAR4
export SIGMADIR=${INPUTNARR}/r2_sigma/$YEAR4
export SFCANLDIR=${INPUTNARR}/r2_sfcanl/$YEAR4
export FLX06DIR=${INPUTNARR}/flx06

mkdir -p $ETABCSDIR
mkdir -p $SIGMADIR
mkdir -p $SFCANLDIR

cp $OUTRCDAS/etabcs00_$DATE12 $ETABCSDIR
cp $OUTRCDAS/etabcs00_$DATE24 $ETABCSDIR
cp sanl$DATE $SIGMADIR/sig.anl.$DATE.ieee
cp sanl$DATE06 $SIGMADIR/sig.anl.$DATE06.ieee
cp sanl$DATE12 $SIGMADIR/sig.anl.$DATE12.ieee
cp sanl$DATE18 $SIGMADIR/sig.anl.$DATE18.ieee
cp sanl$DATE24 $SIGMADIR/sig.anl.$DATE24.ieee
cp sfcanl$DATE $SFCANLDIR/sfc.anl.$DATE.ieee
cp sfcanl$DATE06 $SFCANLDIR/sfc.anl.$DATE06.ieee
cp sfcanl$DATE12 $SFCANLDIR/sfc.anl.$DATE12.ieee
cp sfcanl$DATE18 $SFCANLDIR/sfc.anl.$DATE18.ieee
wgrib grb2d$DATE | grep "kpds5=121:kpds6=1:kpds7=0" | wgrib -i grb2d$DATE -grib -append -o $FLX06DIR/flx.ft06.${YEAR4}${MONTH}
wgrib grb2d$DATE | grep "kpds5=122:kpds6=1:kpds7=0" | wgrib -i grb2d$DATE -grib -append -o $FLX06DIR/flx.ft06.${YEAR4}${MONTH}
wgrib grb2d$DATE06 | grep "kpds5=121:kpds6=1:kpds7=0" | wgrib -i grb2d$DATE06 -grib -append -o $FLX06DIR/flx.ft06.${YEAR4}${MONTH}
wgrib grb2d$DATE06 | grep "kpds5=122:kpds6=1:kpds7=0" | wgrib -i grb2d$DATE06 -grib -append -o $FLX06DIR/flx.ft06.${YEAR4}${MONTH}
wgrib grb2d$DATE12 | grep "kpds5=121:kpds6=1:kpds7=0" | wgrib -i grb2d$DATE12 -grib -append -o $FLX06DIR/flx.ft06.${YEAR4}${MONTH}
wgrib grb2d$DATE12 | grep "kpds5=122:kpds6=1:kpds7=0" | wgrib -i grb2d$DATE12 -grib -append -o $FLX06DIR/flx.ft06.${YEAR4}${MONTH}
wgrib grb2d$DATE18 | grep "kpds5=121:kpds6=1:kpds7=0" | wgrib -i grb2d$DATE18 -grib -append -o $FLX06DIR/flx.ft06.${YEAR4}${MONTH}
wgrib grb2d$DATE18 | grep "kpds5=122:kpds6=1:kpds7=0" | wgrib -i grb2d$DATE18 -grib -append -o $FLX06DIR/flx.ft06.${YEAR4}${MONTH}

### End boundary condition processing

### Prepbufr processing

echo $DATE > curdate
CDATE=`cut -c 1-8 curdate`
export CDATE

cdas=/ptmp/wx52rr/rcdas
exec=/emc2/wx20jw/rrday
temp=/ptmp/wx52rr/rrday
dest=$OUTRCDAS

date=$CDATE; datn=`$exec/nextday $date`

[ -d $temp ] && rm -rf $temp; mkdir -p $temp; cd $temp

#  get the cdas files for this date
#  --------------------------------

for hh in 00 06 12 18 24
do

[ $hh -lt 24 ] && post=$cdas/prepqm${date}$hh.post
[ $hh -eq 24 ] && post=$cdas/prepqm${datn}00.post

[ -s $post.gz  ] && { cp $post.gz prepqm$hh.gz; gunzip prepqm$hh.gz; }
[ -s $post     ] && { cp $post    prepqm$hh;                         }
##[ -s prepqm$hh ] || { echo missing $post; exit 99;                   }
[ -s prepqm$hh ] || { echo missing $post;                   }

done

for hh in 00 03 06 09 12 15 18 21
do

[ $hh -eq 00 ] && { f1=prepqm00; f2=null    ; }
[ $hh -eq 03 ] && { f1=prepqm00; f2=prepqm06; }
[ $hh -eq 06 ] && { f1=prepqm06; f2=null    ; }
[ $hh -eq 09 ] && { f1=prepqm06; f2=prepqm12; }
[ $hh -eq 12 ] && { f1=prepqm12; f2=null    ; }
[ $hh -eq 15 ] && { f1=prepqm12; f2=prepqm18; }
[ $hh -eq 18 ] && { f1=prepqm18; f2=null    ; }
[ $hh -eq 21 ] && { f1=prepqm18; f2=prepqm24; }

#  make a three hour file from one or two six hour files
#  -----------------------------------------------------

ln -sf $f1      fort.20
ln -sf $f2      fort.21
ln -sf rrfile   fort.50
echo $date$hh|$exec/g6tor3x.x

#  modify the global format to a regional equivalent
#  -------------------------------------------------

ln -sf rrfile                   fort.20
ln -sf $dest/prepbufr2.$date$hh fort.50
$exec/grtorr2.x

export err=$?

if [ $err -ne 0 ]; then
  echo "*********************************"
  echo "* $date$hh prepbufr is missing  *"
  echo "*********************************"
fi

export HOLDBUFR=${INPUTNARR}/prepbufr_r2/$YEAR4
mkdir -p $HOLDBUFR

cp $dest/prepbufr2.$date$hh $HOLDBUFR/prepbufr2.$date$hh

done

exit
