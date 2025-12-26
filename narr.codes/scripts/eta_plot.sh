#! /bin/sh

## @ step_name = eta_plot
## @ output = job.$(jobid).$(stepid).$(step_name).out
## @ error = job.$(jobid).$(stepid).$(step_name).err
## @ job_type = serial
##### @ wall_clock_limit = 00:00:00
## @ class = dev
## @ queue

set -aeux

export PS4='$SECONDS + '
echo "$0 STRDATE "`date`

echo "=====ENV====="
env
if [ -z "${LOADL_JOB_NAME:-}" ]; then
  LOADL_JOB_NAME=$$
else
  echo ${LOADL_JOB_NAME} > LLJOB_ID_${LOADL_STEP_NAME}
fi
echo "=====ENV====="

#### logic to find the most recently completed cycle ####
#### put most recently completed cycle in DATEND ####

if [ $# -eq 2 ] ; then
#   STREAM=$1
   DATEND=$2
else
    echo 'Usage: '$0' stream DATE (YYYYMMDDCC) ' 1>&2
    exit 1
fi


if [ ${#DATEND} -ne 10 ]; then
    echo 'Usage: '$0' DATEND (YYYYMMDDCC) ' 1>&2
    exit 1
fi

echo ${NARR:?"NARR is not set; fatal error"}

readonly STREAM
echo ${STREAM:?"STREAM is not set; fatal error"}

. ${NARR}/scr/narr_envir.sh

YEAR=`expr substr $DATEND 1 4`

export RUNPLOT=${TMPNARR}/run_${YEAR}/plot_${DATEND}
export GRIDTOB=${TMPNARR}/stats_test00/
export GRIDTOGRID=${TMPNARR}/vsdb/
export CMPEVN=${TMPNARR}/cmpevn

mkdir -p $RUNPLOT
cd $RUNPLOT

echo $DATEND > curdate2
CDATEND=`cut -c 1-8 curdate2`
CENTEND=`cut -c 1-2 curdate2`
YEAR4END=`cut -c 1-4 curdate2`
YEAR2END=`cut -c 3-4 curdate2`
MONTHEND=`cut -c 5-6 curdate2`
DAYEND=`cut -c 7-8 curdate2`
export CDATEND YEAR4END YEAR2END MONTHEND DAYEND CENTEND

export DAYLEN=365
export HRLEN=`echo "$DAYLEN*24 \n quit" | bc`
echo $DAYLEN $HRLEN

DATEBEG=`$UTIL/ndate -$HRLEN $DATEND`  
# change 720 to 2168 for a 90-day monitoring #

export DATEBEG
echo $DATEBEG
echo $DATEBEG > curdate
CDATE=`cut -c 1-8 curdate`
CENT=`cut -c 1-2 curdate`
YEAR4=`cut -c 1-4 curdate`
YEAR2=`cut -c 3-4 curdate`
MONTH=`cut -c 5-6 curdate`
DAY=`cut -c 7-8 curdate`
export CDATE YEAR4 YEAR2 MONTH DAY CENT

##############
# GRID-TO-OB #
##############

echo $YEAR4END $MONTHEND $DAYEND $DAYLEN $GRIDTOB | $EXEC/readvsdb.x > readvsdb.out

#set -A mon jan feb mar apr may jun jul aug sep oct nov dec
#let mn=$MONTH-1
#mname=${mon[mn]}
#echo $mname

case $MONTH in
   01) mname=Jan;;
   02) mname=Feb;;
   03) mname=Mar;;
   04) mname=Apr;;
   05) mname=May;;
   06) mname=Jun;;
   07) mname=Jul;;
   08) mname=Aug;;
   09) mname=Sep;;
   10) mname=Oct;;
   11) mname=Nov;;
   12) mname=Dec;;
    *) echo "month error $mo"
       exit 1;;
esac

cat > vsdb.ctl << EOF
dset ^out5.grd
undef 9.999E+6
title 4x daily flx file from NCEP/DOE Reanalysis (R-2)
xdef 1 linear 1 1
ydef 1 linear 1 1
zdef 1 linear 1 1
tdef $DAYLEN linear 00Z${DAY}${mname}${YEAR4} 24hr
vars 29
t850f   0 99 mean [f] of T (p=850)
t850o   0 99 mean [o] of T (p=850)
t850fo  0 99 mean [f*o] of T (p=850)
t850f2  0 99 mean [f**2] of T (p=850)
t850o2  0 99 mean [o**2] of T (p=850)
t200f   0 99 mean [f] of T (p=200)
t200o   0 99 mean [o] of T (p=200)
t200fo  0 99 mean [f*o] of T (p=200)
t200f2  0 99 mean [f**2] of T (p=200)
t200o2  0 99 mean [o**2] of T (p=200)
rh850f  0 99 mean [f] of RH (p=850)
rh850o  0 99 mean [o] of RH (p=850)
rh850fo 0 99 mean [f*o] of RH (p=850)
rh850f2 0 99 mean [f**2] of RH (p=850)
rh850o2 0 99 mean [o**2] of RH (p=850)
u850f   0 99 mean [uf] (p=850)
v850f   0 99 mean [vf] (p=850)
u850o   0 99 mean [uo] (p=850)
v850o   0 99 mean [vo] (p=850)
uv850fo 0 99 mean[uf*uo+vf*vo] (p=850)
uv850f2 0 99 mean[uf**2+vf**2] (p=850)
uv850o2 0 99 mean[uo**2+vo**2] (p=850)
u200f   0 99 mean [uf] (p=200)
v200f   0 99 mean [vf] (p=200)
u200o   0 99 mean [uo] (p=200)
v200o   0 99 mean [vo] (p=200)
uv200fo 0 99 mean[uf*uo+vf*vo] (p=200)
uv200f2 0 99 mean[uf**2+vf**2] (p=200)
uv200o2 0 99 mean[uo**2+vo**2] (p=200)
ENDVARS
EOF

/usrx/local/grads/bin/grads -bpc "run ${NARR}/sorc/monitoring/13val.gs" > grads.print
/usrx/local/grads/bin/gxps -i 13val.gmf -o 13val.ps
## or create a .gif or .png using appropriate tools (Wesley)

################
# GRID-TO GRID #
################

echo $YEAR4END $MONTHEND $DAYEND $DAYLEN $GRIDTOGRID | $EXEC/readvsdblike.x > readv.out

# FLUXLEN should be 4 times DAYLEN
#export FLUXLEN=120  
#export FLUXLEN = `expr $DAYLEN \* 4`
export FLUXLEN=`echo "$DAYLEN*4 \n quit" | bc`
echo $FLUXLEN

cat > vsdblike_03-21.ctl << EOF
dset ^vsdblike_03-21.grd
undef 9.999E+6
title 4x daily grib-to-grib VSDB files (verifying time: 03,09,15,21)
xdef 1 linear 1 1
ydef 1 linear 1 1
zdef 5 linear 1 1
tdef $FLUXLEN linear 03Z${DAY}${mname}${YEAR4} 06hr
vars 2
LHTFL  5 99 surface latent heat flux
SHTFL  5 99 surface sensible heat flux
ENDVARS
EOF

cat > vsdblike_06-00.ctl << EOF
dset ^vsdblike_06-00.grd
undef 9.999E+6
title 4x daily grib-to-grib VSDB files (verifying time: 06,12,18,00)
xdef 1 linear 1 1
ydef 1 linear 1 1
zdef 5 linear 1 1
tdef $FLUXLEN linear 06Z${DAY}${mname}${YEAR4} 06hr
vars 2
LHTFL  5 99 surface latent heat flux
SHTFL  5 99 surface sensible heat flux
ENDVARS
EOF

/usrx/local/grads/bin/grads -bpc "run ${NARR}/sorc/monitoring/plotvsdblike.gs" > grads2.print
/usrx/local/grads/bin/gxps -i plotvsdblike.gmf -o g2g.ps
## or .gif or .png

#################
# QC MONITORING #
#################

rm -f cmpevn

DATE=$DATEBEG
while [ $DATE -le $DATEND ] ; do

if [ -s $CMPEVN/cmpevn.$DATE ]; then
cat $CMPEVN/cmpevn.$DATE >> cmpevn
else
cat << EOF > dumcmp
yyyymmddhh quan obtype  tot obs | analyzed |  r-2 rej |  3dv rej | filtered |

$DATE PRES ADPUPA        0 |        0 |        0 |        0 |        0 |
$DATE PRES ADPSFC        0 |        0 |        0 |        0 |        0 |
$DATE PRES SFCSHP        0 |        0 |        0 |        0 |        0 |
$DATE PRES SFCBOG        0 |        0 |        0 |        0 |        0 |

$DATE SPHU ADPUPA        0 |        0 |        0 |        0 |        0 |
$DATE SPHU ADPSFC        0 |        0 |        0 |        0 |        0 |
$DATE SPHU SFCSHP        0 |        0 |        0 |        0 |        0 |

$DATE TEMP ADPUPA        0 |        0 |        0 |        0 |        0 |
$DATE TEMP AIRCFT        0 |        0 |        0 |        0 |        0 |
$DATE TEMP ADPSFC        0 |        0 |        0 |        0 |        0 |
$DATE TEMP SFCSHP        0 |        0 |        0 |        0 |        0 |

$DATE WIND ADPUPA        0 |        0 |        0 |        0 |        0 |
$DATE WIND AIRCFT        0 |        0 |        0 |        0 |        0 |
$DATE WIND SATWND        0 |        0 |        0 |        0 |        0 |
$DATE WIND ADPSFC        0 |        0 |        0 |        0 |        0 |
$DATE WIND SFCSHP        0 |        0 |        0 |        0 |        0 |

EOF
cat dumcmp >> cmpevn
fi

DATE=`/nwprod/util/exec/ndate +03 $DATE`

done

$EXEC/qc3.x > qc3.out

#set -A mon jan feb mar apr may jun jul aug sep oct nov dec
#let mn=$MONTH-1
#mname=${mon[mn]}
#echo $mname

case $MONTH in
   01) mname=Jan;;
   02) mname=Feb;;
   03) mname=Mar;;
   04) mname=Apr;;
   05) mname=May;;
   06) mname=Jun;;
   07) mname=Jul;;
   08) mname=Aug;;
   09) mname=Sep;;
   10) mname=Oct;;
   11) mname=Nov;;
   12) mname=Dec;;
    *) echo "month error $mo"
       exit 1;;
esac

cat > qc.ctl << EOF
dset ^qcBinary.out
undef -999
title sample qc monitoring data set
xdef 1 linear 1 1
ydef 1 linear 1 1
tdef $DAYLEN linear 00Z${DAY}${mname}${YEAR4} 24hr
zdef 1 linear 1 1
vars 12
WCadp  0 99 ** total counts of adpupa obs available for wind at 00hr plus 12hr
WPadp  0 99 ** percentage rejected(all causes)
TCadp  0 99 ** total counts of adpupa obs available for temperature at 00hr plus 12hr
TPadp  0 99 ** percentage rejected(all causes)
QCadp  0 99 ** total counts of adpupa obs available for specific humidity at 00hr plus 12hr
QPadp  0 99 ** percentage rejected(all causes)
WCair  0 99 ** total counts of aircft obs available for wind summed over 00,03,06,09,12,15,18,21
WPair  0 99 ** percentage rejected(all causes)
TCair  0 99 ** total counts of aircft obs available for temperature summed over 00,03,06,09,12,15,18,21
TPair  0 99 ** percentage rejected(all causes)
WTQCsfc  0 99 ** total counts of adpsfc obs available for wind plus temperature plus specific humidity summed over 00,03,06,09,12,15,18,21
WTQPsfc  0 99 ** percentage rejected(all causes)
ENDVARS
EOF

/usrx/local/grads/bin/grads -bpc "run ${NARR}/sorc/monitoring/qcplot.gs" > grads3.print
/usrx/local/grads/bin/gxps -i qcplot.gmf -o qcplot.ps

###########################
# PRECIP ASSIM MONITORING #
###########################

rm -f pcpstats
rm -f printpcp

export DAYLEN=30
export HRLEN=`echo "$DAYLEN*24 \n quit" | bc`
export HRLEN4=`echo "$DAYLEN*8 \n quit" | bc`
echo $DAYLEN $HRLEN $HRLEN4

DATEBEG=`$UTIL/ndate -$HRLEN $DATEND`

export DATE=$DATEBEG

echo $DATEBEG > curdate
YEARB=`cut -c 1-4 curdate`
MONTHB=`cut -c 5-6 curdate`
DAYB=`cut -c 7-8 curdate`
HRB=`cut -c 9-10 curdate`
export YEARB MONTHB DAYB HRB

while [ $DATE -le $DATEND ]; do

echo $DATE > curdate
YEAR=`cut -c 1-4 curdate`
MONTH=`cut -c 5-6 curdate`
DAY=`cut -c 7-8 curdate`
HR=`cut -c 9-10 curdate`
export YEAR MONTH DAY HR

DATM12=`$UTIL/ndate -12 $DATE`
echo $DATM12 > curdate12
YEAR12=`cut -c 1-4 curdate12`
MONTH12=`cut -c 5-6 curdate12`
DAY12=`cut -c 7-8 curdate12`
HR12=`cut -c 9-10 curdate12`
export CDATE YEAR4 YEAR2 MONTH DAY CENT
DATM09=`$UTIL/ndate -09 $DATE`
echo $DATM09 > curdate09
YEAR09=`cut -c 1-4 curdate09`
MONTH09=`cut -c 5-6 curdate09`
DAY09=`cut -c 7-8 curdate09`
HR09=`cut -c 9-10 curdate09`
DATM06=`$UTIL/ndate -06 $DATE`
echo $DATM06 > curdate06
YEAR06=`cut -c 1-4 curdate06`
MONTH06=`cut -c 5-6 curdate06`
DAY06=`cut -c 7-8 curdate06`
HR06=`cut -c 9-10 curdate06`
DATM03=`$UTIL/ndate -03 $DATE`
echo $DATM03 > curdate03
YEAR03=`cut -c 1-4 curdate03`
MONTH03=`cut -c 5-6 curdate03`
DAY03=`cut -c 7-8 curdate03`
HR03=`cut -c 9-10 curdate03`
export DATM12 DATM09 DATM06 DATM03
export YEAR12 YEAR09 YEAR06 YEAR03
export MONTH12 MONTH09 MONTH06 MONTH03
export DAY12 DAY09 DAY06 DAY03
export HR12 HR09 HR06 HR03

export RRDIR12=${TMPNARR}/output_$YEAR12/precip/${YEAR12}${MONTH12}
export RRDIR09=${TMPNARR}/output_$YEAR09/precip/${YEAR09}${MONTH09}
export RRDIR06=${TMPNARR}/output_$YEAR06/precip/${YEAR06}${MONTH06}
export RRDIR03=${TMPNARR}/output_$YEAR03/precip/${YEAR03}${MONTH03}
export OBSDIR=/gpfsuser/narr/input/pcpdata_prod/$DATE

cp ${NARR}/sorc/monitoring/egrdland.grb .
export EGRD=egrdland.grb

wgrib -s $OBSDIR/egrd80.tm12.3hr | awk '{if ($1 > 0 && $1 < 2) print $0}' FS=':' | wgrib -i $OBSDIR/egrd80.tm12.3hr -grib -o pcpobs12.grb
wgrib -s $OBSDIR/egrd80.tm12.3hr | awk '{if ($1 > 1 && $1 < 3) print $0}' FS=':' | wgrib -i $OBSDIR/egrd80.tm12.3hr -grib -o pcpobs11.grb
wgrib -s $OBSDIR/egrd80.tm12.3hr | awk '{if ($1 > 2 && $1 < 4) print $0}' FS=':' | wgrib -i $OBSDIR/egrd80.tm12.3hr -grib -o pcpobs10.grb

wgrib -s $OBSDIR/egrd80.tm09.3hr | awk '{if ($1 > 0 && $1 < 2) print $0}' FS=':' | wgrib -i $OBSDIR/egrd80.tm09.3hr -grib -o pcpobs9.grb
wgrib -s $OBSDIR/egrd80.tm09.3hr | awk '{if ($1 > 1 && $1 < 3) print $0}' FS=':' | wgrib -i $OBSDIR/egrd80.tm09.3hr -grib -o pcpobs8.grb
wgrib -s $OBSDIR/egrd80.tm09.3hr | awk '{if ($1 > 2 && $1 < 4) print $0}' FS=':' | wgrib -i $OBSDIR/egrd80.tm09.3hr -grib -o pcpobs7.grb

wgrib -s $OBSDIR/egrd80.tm06.3hr | awk '{if ($1 > 0 && $1 < 2) print $0}' FS=':' | wgrib -i $OBSDIR/egrd80.tm06.3hr -grib -o pcpobs6.grb
wgrib -s $OBSDIR/egrd80.tm06.3hr | awk '{if ($1 > 1 && $1 < 3) print $0}' FS=':' | wgrib -i $OBSDIR/egrd80.tm06.3hr -grib -o pcpobs5.grb
wgrib -s $OBSDIR/egrd80.tm06.3hr | awk '{if ($1 > 2 && $1 < 4) print $0}' FS=':' | wgrib -i $OBSDIR/egrd80.tm06.3hr -grib -o pcpobs4.grb

wgrib -s $OBSDIR/egrd80.tm03.3hr | awk '{if ($1 > 0 && $1 < 2) print $0}' FS=':' | wgrib -i $OBSDIR/egrd80.tm03.3hr -grib -o pcpobs3.grb
wgrib -s $OBSDIR/egrd80.tm03.3hr | awk '{if ($1 > 1 && $1 < 3) print $0}' FS=':' | wgrib -i $OBSDIR/egrd80.tm03.3hr -grib -o pcpobs2.grb
wgrib -s $OBSDIR/egrd80.tm03.3hr | awk '{if ($1 > 2 && $1 < 4) print $0}' FS=':' | wgrib -i $OBSDIR/egrd80.tm03.3hr -grib -o pcpobs1.grb

cp ${RRDIR12}/EGDAWP.APCP.$DATM12 .
cp ${RRDIR09}/EGDAWP.APCP.$DATM09 .
cp ${RRDIR06}/EGDAWP.APCP.$DATM06 .
cp ${RRDIR03}/EGDAWP.APCP.$DATM03 .

#set -A mon JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC
#let mn=MONTH09-1
#mname=${mon[mn]}
#echo $mname

case $MONTH09 in
   01) mname=JAN;;
   02) mname=FEB;;
   03) mname=MAR;;
   04) mname=APR;;
   05) mname=MAY;;
   06) mname=JUN;;
   07) mname=JUL;;
   08) mname=AUG;;
   09) mname=SEP;;
   10) mname=OCT;;
   11) mname=NOV;;
   12) mname=DEC;;
    *) echo "month error $mo"
       exit 1;;
esac

cat >> pcpstats << EOF
${HR09}Z${DAY09}${mname}${YEAR09}
EOF

ln -s -f EGDAWP.APCP.$DATM12 fort.10
ln -s -f pcpobs12.grb fort.21
ln -s -f pcpobs11.grb fort.22
ln -s -f pcpobs10.grb fort.23
ln -s -f $EGRD fort.30
ln -s -f pcpstats.out fort.50
$EXEC/pcpmon.x >> printpcp

cat pcpstats.out >> pcpstats

#set -A mon JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC
#let mn=MONTH06-1
#mname=${mon[mn]}
#echo $mname

case $MONTH06 in
   01) mname=JAN;;
   02) mname=FEB;;
   03) mname=MAR;;
   04) mname=APR;;
   05) mname=MAY;;
   06) mname=JUN;;
   07) mname=JUL;;
   08) mname=AUG;;
   09) mname=SEP;;
   10) mname=OCT;;
   11) mname=NOV;;
   12) mname=DEC;;
    *) echo "month error $mo"
       exit 1;;
esac

cat >> pcpstats << EOF
${HR06}Z${DAY06}${mname}${YEAR06}
EOF

ln -s -f EGDAWP.APCP.$DATM09 fort.10
ln -s -f pcpobs9.grb fort.21
ln -s -f pcpobs8.grb fort.22
ln -s -f pcpobs7.grb fort.23
ln -s -f $EGRD fort.30
ln -s -f pcpstats.out fort.50
$EXEC/pcpmon.x >> printpcp

cat pcpstats.out >> pcpstats

#set -A mon JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC
#let mn=MONTH03-1
#mname=${mon[mn]}
#echo $mname

case $MONTH03 in
   01) mname=JAN;;
   02) mname=FEB;;
   03) mname=MAR;;
   04) mname=APR;;
   05) mname=MAY;;
   06) mname=JUN;;
   07) mname=JUL;;
   08) mname=AUG;;
   09) mname=SEP;;
   10) mname=OCT;;
   11) mname=NOV;;
   12) mname=DEC;;
    *) echo "month error $mo"
       exit 1;;
esac

cat >> pcpstats << EOF
${HR03}Z${DAY03}${mname}${YEAR03}
EOF

ln -s -f EGDAWP.APCP.$DATM06 fort.10
ln -s -f pcpobs6.grb fort.21
ln -s -f pcpobs5.grb fort.22
ln -s -f pcpobs4.grb fort.23
ln -s -f $EGRD fort.30
ln -s -f pcpstats.out fort.50
$EXEC/pcpmon.x >> printpcp

cat pcpstats.out >> pcpstats

#set -A mon JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC
#let mn=MONTH-1
#mname=${mon[mn]}
#echo $mname

case $MONTH in
   01) mname=JAN;;
   02) mname=FEB;;
   03) mname=MAR;;
   04) mname=APR;;
   05) mname=MAY;;
   06) mname=JUN;;
   07) mname=JUL;;
   08) mname=AUG;;
   09) mname=SEP;;
   10) mname=OCT;;
   11) mname=NOV;;
   12) mname=DEC;;
    *) echo "month error $mo"
       exit 1;;
esac

cat >> pcpstats << EOF
${HR}Z${DAY}${mname}${YEAR}
EOF

ln -s -f EGDAWP.APCP.$DATM03 fort.10
ln -s -f pcpobs3.grb fort.21
ln -s -f pcpobs2.grb fort.22
ln -s -f pcpobs1.grb fort.23
ln -s -f $EGRD fort.30
ln -s -f pcpstats.out fort.50
$EXEC/pcpmon.x >> printpcp

cat pcpstats.out >> pcpstats
DATE=`/nwprod/util/exec/ndate +12 $DATE`

rm -f EGDAWP.APCP.$DATM12  
rm -f EGDAWP.APCP.$DATM09  
rm -f EGDAWP.APCP.$DATM06  
rm -f EGDAWP.APCP.$DATM03  

done

echo $HRLEN4 | $EXEC/pcp.x > printpcpx

#set -A mon JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC
#let mn=MONTHB-1
#mname=${mon[mn]}
#echo $mname

case $MONTHB in
   01) mname=Jan;;
   02) mname=Feb;;
   03) mname=Mar;;
   04) mname=Apr;;
   05) mname=May;;
   06) mname=Jun;;
   07) mname=Jul;;
   08) mname=Aug;;
   09) mname=Sep;;
   10) mname=Oct;;
   11) mname=Nov;;
   12) mname=Dec;;
    *) echo "month error $mo"
       exit 1;;
esac

cat > pcpmon.ctl << EOF
dset ^pcp.out
undef 9.999E+20
title sample precipitation data set
xdef 1 linear 1 1
ydef 1 linear 1 1
tdef ${HRLEN4} linear ${HRB}Z${DAYB}${mname}${YEARB} 3hr
zdef 1 linear 1 1
vars 12
obdm  0 99 ** mean observation precipitation domain
obla  0 99 ** mean observation precipitation over land
oboc  0 99 ** mean observation precipitation over ocean
rrdm  0 99 ** mean RR precipitation domain
rrla  0 99 ** mean RR precipitation over land
rroc  0 99 ** mean RR precipitation over ocean
bidm  0 99 ** biases domain
bila  0 99 ** biases over land
bioc  0 99 ** biases over ocean
sddm  0 99 ** standard deviations domain
sdla  0 99 ** standard deviations over land
sdoc  0 99 ** standard deviations over ocean
ENDVARS
EOF

/usrx/local/grads/bin/grads -bpc "run ${NARR}/sorc/monitoring/pcplaoc.gs" > grads.print4
/usrx/local/grads/bin/gxps -c -i pcplaoc.gmf -o pcplaoc.ps

rm -f EGDAWP.APCP*
rm -f pcpobs*
rm -f $EGRD

#

echo "$0 ENDDATE "`date`

exit 0
