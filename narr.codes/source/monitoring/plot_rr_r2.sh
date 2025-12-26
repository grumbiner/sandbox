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

gscript=/gpfsuser/narr/sorc/monitoring/plot_rr_r2.gs

set -x

# figure out names of eta analysis and forecasts
#   $eta = analysis  $fcst3 = 3hr fcst, $fcst6 = 6 hr fcst

# input grib files

eta=$1

fcst3=`echo $eta | sed "s/AWIP3200/AWIP3203/"`
date=`wgrib -4yr -s -d 1 $eta -o /dev/null | cut -f3 -d: | sed 's/d=//'`
date6=`$ndate -6 $date`
datep3=`$ndate +3 $date`
fcst6=`echo $fcst3 | sed "s/$date/$datep3/"`


# output grib files

outll=/u/wx51we/home/rr/rr_r2_data/eta00.ll.$date
out0=/u/wx51we/home/rr/rr_r2_data/eta00.gau.$date
out3=/u/wx51we/home/rr/rr_r2_data/eta03.gau.$date
out6=/u/wx51we/home/rr/rr_r2_data/eta06.gau.$date


if [ 1 -eq 1 ] ; then

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


# make the control and index files
/u/wx51we/bin/grib2ctl.pl -rr -ts6hr $outll >$outll.ctl
/u/wx51we/bin/grib2ctl.pl -rr -ts6hr $out0 >$out0.ctl
/u/wx51we/bin/grib2ctl.pl -rr -ts6hr $out3 >$out3.ctl
/u/wx51we/bin/grib2ctl.pl -rr -ts6hr $out6 >$out6.ctl

gribmap -0 -i $outll.ctl
gribmap -0 -i $out0.ctl
gribmap -0 -i $out3.ctl
gribmap -0 -i $out6.ctl

fi


date6=`$ndate -6 $date`
yyyymm6=`echo $date6 | cut -c1-6`
yyyy=`echo $date | cut -c1-4`
yyyymm=`echo $date | cut -c1-6`

# now to make r2 pgb file
sigma=/gpfsuser/narr/input/r2_sigma/$yyyy/sig.anl.$date.ieee
pgb=pgb$date.grb

if [ ! -f $sigma ] ; then
   echo "no sigma file for $date"
   exit 8
fi

export EXPBINDIR=/u/wx51we/home/r2/nmmsave/rerun_ctl/bin
export UTILS=/u/wx51we/home/r1/utils

# make double precision sanl

$UTILS/sig2dbl $sigma sanl$date.dbl

touch fort.00
rm fort.*
[ -f $pgb ] && rm $pgb

tmpdir='.'

INPUT=$tmpdir/pgb.parm
echo "   &NAMPGB" >$INPUT
echo "   ICEN2=1,">>$INPUT
echo "   IGEN=195,">>$INPUT
echo "/" >>$INPUT

#

ln -fs sanl$date.dbl                   fort.11
ln -fs $pgb                            fort.51


$EXPBINDIR/pgb.x <$INPUT

rm sanl$date.dbl

$grib2ctl $pgb >$pgb.ctl
gribmap -0 -i $pgb.ctl

$grads -bl <<EOF
open $outll.ctl
open $pgb.ctl
open $out0.ctl
open $r2flxdir/flx.ft06.$yyyymm6.ctl
open $out3.ctl
open $out6.ctl
open $r2flxdir/flx.ft06.$yyyymm.ctl
open /u/wx51we/home/rr/4plot/r2-landGau.sfc.ctl
$gscript
EOF
set +x

echo "gradz $outll.ctl $pgb.ctl $out0.ctl $r2flxdir/flx.ft06.$yyyymm6.ctl $out3.ctl $out6.ctl $r2flxdir/flx.ft06.$yyyymm.ctl /u/wx51we/home/rr/4plot/r2-landGau.sfc.ctl "


