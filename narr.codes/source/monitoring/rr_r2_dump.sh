#!/bin/sh

# make rr vs r2 plots
copygb=/nwprod/util/exec/copygb
wgrib=/u/wx51we/bin/wgrib
grib2ctl=/u/wx51we/bin/grib2ctl.pl
ndate=/nwprod/util/exec/ndate
grads=/u/wx51we/bin/grads
r2flxdir=/u/wx51we/home/rr/r2_flx
export GADDIR=/usrx/local/grads/dat
export GASCRP=/u/wx51we/home/grads

set -x

# figure out names of eta analysis and forecasts
#   $eta = analysis  $fcst3 = 3hr fcst, $fcst6 = 6 hr fcst

# input grib files

eta=$1

##eta=/gpfsuser/narr/output/s4/output_1998/edas_1998112612/AWIP3200.1998112600

fcst3=`echo $eta | sed "s/AWIP3200/AWIP3203/"`
date=`wgrib -4yr -s -d 1 $eta -o /dev/null | cut -f3 -d: | sed 's/d=//'`


hr=`echo $date | cut -c 9-10`
[ `expr $hr % 6` -ne 0 ] && exit

date6=`$ndate -6 $date`
datep3=`$ndate +3 $date`
fcst6=`echo $fcst3 | sed "s/$date/$datep3/"`


# output grib files

outll=/u/wx51we/home/rr/rr_r2_data/eta00.ll.$date
out0=/u/wx51we/home/rr/rr_r2_data/eta00.gau.$date
out3=/u/wx51we/home/rr/rr_r2_data/eta03.gau.$date
out6=/u/wx51we/home/rr/rr_r2_data/eta06.gau.$date


#                make lat-long analyses

$wgrib -s $eta | \
  egrep ":((HGT|TMP|SPFH|UGRD|VGRD):(850|700|500|200) mb|MSLET:MSL):" \
    | $wgrib -i $eta -grib -s -o $outll.tmp
$copygb -g2 -x $outll.tmp $outll
rm $outll.tmp


#                make 0 hour gaussian grid fields

$wgrib -s $eta | \
   egrep ":((PRES|RH|TMP|SPFH):2 m above gnd|(UGRD|VGRD):10 m above gnd|SOILW):" \
    | $wgrib -i $eta -grib -s -o $out0.tmp
$copygb -g98 -x $out0.tmp $out0
rm $out0.tmp


#               make 3 hour flux files
$wgrib -s $fcst3 | \
   egrep ":(LHTFL|SHTFL|APCP):sfc:0-3hr a" \
   | wgrib -i $fcst3 -grib -s -o $out3.tmp
$copygb -g98 -x $out3.tmp $out3
rm $out3.tmp

$wgrib -s $fcst6 | \
   egrep ":(LHTFL|SHTFL|APCP):sfc:0-3hr a" \
   | wgrib -i $fcst6 -grib -s -o $out6.tmp
$copygb -g98 -x $out6.tmp $out6
rm $out6.tmp

exit 0
