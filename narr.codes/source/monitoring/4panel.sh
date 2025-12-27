#!/bin/sh

if [ $# -ne 3 ] ; then
   echo "usage: $0 analysis-grib file fcst-grib file"
   echo " produces 2 4 panel charts"
   exit 8
fi

set -x

stream=$3

###if [ $stream = s3 ] ; then
### 
### . ${NARR}/sorc/monitoring/rr_merge_awip.sh $1 $2 $1.merged 
###
###fi

[ -f page1.ps ] && rm page1.ps
[ -f page2.ps ] && rm page2.ps

grib2ctl=/u/wx51we/bin/grib2ctl.pl
wgrib=/u/wx51we/bin/wgrib
#panel=/u/wx51we/home/rr/4plot/4panel.gs
panel=${NARR}/sorc/monitoring/4panel.gs
gribmap=/u/wx51we/bin/gribmap
grads=/u/wx51we/bin/grads
GADDIR=/usrx/local/grads/dat
GASCRP=/u/wx51we/home/grads

analysis=$1.grads
fcst=$2.grads

# make grads compatible files

$wgrib -s $2 | grep -v ":WEASD:sfc:0-3hr acc:" | \
 egrep -v ":[UD][LS]WRF:sfc:3hr fcst:" | \
 egrep -v ":[LS]HTFL:sfc:3hr fcst:" | \
 egrep -v ":GFLUX:sfc:3hr fcst:" | \
  $wgrib -s $2 -i -o $fcst -grib

cp $1 $analysis

#CTL=/u/wx51we/home/rr/4plot/AWIP.ctl.org
CTL=${NARR}/sorc/monitoring/AWIP.ctl.org

export grib2ctl panel gribmap grads GADDIR GASCRP

# make ctl file from scratch (slow)
#$grib2ctl $1 >$1.ctl
#$grib2ctl $2 >$2.ctl

date=`$wgrib -d 1 $1 -4yr | cut -f3 -d: | sed 's/d=//'`
year=`echo $date | cut -c1-4`
mo=`echo $date | cut -c5-6`
day=`echo $date | cut -c7-8`
hr=`echo $date | cut -c9-10`
case $mo in
   01) month=jan;;
   02) month=feb;;
   03) month=mar;;
   04) month=apr;;
   05) month=may;;
   06) month=jun;;
   07) month=jul;;
   08) month=aug;;
   09) month=sep;;
   10) month=oct;;
   11) month=nov;;
   12) month=dec;;
    *) echo "month error $mo"
       exit 1;;
esac
gradsdate="${hr}Z$day$month$year"


sed -e "s:((file)):$analysis:" -e "s:((date)):$gradsdate:" $CTL >$analysis.ctl
sed -e "s:((file)):$fcst:" -e "s:((date)):$gradsdate:" $CTL >$fcst.ctl

$gribmap -0 -i $analysis.ctl
$gribmap -0 -i $fcst.ctl

$grads -bl <<EOF
open $analysis.ctl
open $fcst.ctl
run $panel
EOF
