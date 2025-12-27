#! /bin/sh

YEAR=`cut -c 1-4 curdate`
CDAT=`cut -c 1-8 curdate`
export YEAR CDAT

DAY=`cut -c 7-8 curdate`
CYC=`cut -c 9-10 curdate`

export DATM12=`$UTIL/ndate -12 $DATE`
echo $DATM12 > datetm12
CYCM12=`cut -c 9-10 datetm12`

export DATM09=`$UTIL/ndate -09 $DATE`
echo $DATM09 > datetm09
CYCM09=`cut -c 9-10 datetm09`

export DATM06=`$UTIL/ndate -06 $DATE`
echo $DATM06 > datetm06
CYCM06=`cut -c 9-10 datetm06`

export DATM03=`$UTIL/ndate -03 $DATE`
echo $DATM03 > datetm03
CYCM03=`cut -c 9-10 datetm03`

DATE03=`$UTIL/ndate 03 $DATE`
DATE06=`$UTIL/ndate 06 $DATE`
# echo $DATE06 > date06
DATE12=`$UTIL/ndate 12 $DATE`
# echo $DATE12 > date12
DATE18=`$UTIL/ndate 18 $DATE`
# echo $DATE18 > date18
DATE24=`$UTIL/ndate 24 $DATE`
# echo $DATE24 > date24
DATE30=`$UTIL/ndate 30 $DATE`
# echo $DATE30 > date30
DATE36=`$UTIL/ndate 36 $DATE`
# echo $DATE36 > date36
DATE42=`$UTIL/ndate 42 $DATE`
# echo $DATE42 > date42
DATE48=`$UTIL/ndate 48 $DATE`
# echo $DATE48 > date48
DATE54=`$UTIL/ndate 54 $DATE`
# echo $DATE54 > date54
DATE60=`$UTIL/ndate 60 $DATE`
# echo $DATE60 > date60
DATE66=`$UTIL/ndate 66 $DATE`
# echo $DATE66 > date66
DATE72=`$UTIL/ndate 72 $DATE`
# echo $DATE72 > date72

export CDAT1=`cut -c 1-8 datetm12`

export CDATM12=`cut -c 3-10 datetm12`
export CDATM09=`cut -c 3-10 datetm09`
export CDATM06=`cut -c 3-10 datetm06`
export CDATM03=`cut -c 3-10 datetm03`
CDATE00=`cut -c 3-10 curdate`

# CDATE06=`cut -c 3-10 date06`
# CDATE12=`cut -c 3-10 date12`
# CDATE18=`cut -c 3-10 date18`
# CDATE24=`cut -c 3-10 date24`
# CDATE30=`cut -c 3-10 date30`
# CDATE36=`cut -c 3-10 date36`
# CDATE42=`cut -c 3-10 date42`
# CDATE48=`cut -c 3-10 date48`
# CDATE54=`cut -c 3-10 date54`
# CDATE60=`cut -c 3-10 date60`
# CDATE66=`cut -c 3-10 date66`
# CDATE72=`cut -c 3-10 date72`
