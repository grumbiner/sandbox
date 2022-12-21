#Interpolate from the NASA grid SMMR/SSMI climatology of concentration
# to the NICE grid, north.

dd=`date +'%d'`
mm=`date +'%m'`
yy=`date +'%y'`
dnew=`expr $dd - 1`

ndir=$HOME/ssmi/climate/north/
sdir=$HOME/ssmi/climate/south/
ntrail=n.cmr
strail=s.cmr

touch conc.north conc.south date

#set -xe

for incr in 0 1 2 3 4 5 6 7
do
  dnew=`expr $dnew + 1`
  if [ $dnew -gt 32 ] ; then
    dd=1
    dnew=1
    mm=`expr $mm + 1`
    if [ $mm -ge 13 ] ; then
      mm=`expr $mm - 12 `
      yy=`expr $yy + 1 `
    fi
  fi
  if [ \( $dnew -eq 31 \) -a \( $mm -eq 2 -o $mm -eq 4 -o $mm -eq 6 -o  \
                         $mm -eq 9 -o $mm -eq 11 \) ] ; then
    dd=1
    dnew=1
    mm=`expr $mm + 1`
    if [ $mm -ge 13 ] ; then
      mm=`expr $mm - 12 `
      yy=`expr $yy + 1 `
    fi
  fi
  if [ $dnew -eq 30 -a $mm -eq 2 ] ; then
    dd=1
    dnew=1
    mm=`expr $mm + 1`
    if [ $mm -ge 13 ] ; then
      mm=`expr $mm - 12 `
      yy=`expr $yy + 1 `
    fi
  fi
  if [ $dnew -eq 29 -a $mm -eq 2 ] ; then
    echo 'Leap year control not yet written, using feb 29th anyhow'
  fi
  if [ $dnew -lt 10 ] ; then
    dnew=`echo 0$dnew`
  fi

  echo $mm $dnew 
  nasanh ${ndir}${mm}${dnew}$ntrail n${mm}${dnew}
  nasash ${sdir}${mm}${dnew}$strail s${mm}${dnew}

  if [ $incr -eq 0 ] ; then
    cat n${mm}${dnew} > conc.north
    cat s${mm}${dnew} > conc.south
    echo 19${yy}${mm}${dnew} > date
   else
    cat n${mm}${dnew} >> conc.north
    cat s${mm}${dnew} >> conc.south
    echo 19${yy}${mm}${dnew} >> date
  fi
  
done 
