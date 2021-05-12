#!/bin/sh
## This script pulls the data you need for plotting the COFS Native-Grid-GRIB data 
## using IDL
#
# It needs 6 arguements as input. Maintain the uppercase as shown in the example below
#
if [ $# -ne 6 ]; then
 echo
 echo
 echo Warning: Wrong number of arguements
 echo
 echo Args: 1.grib_fn 2.SIGMA/Z 3.layer/levelnum. 4.now/forecast 5.YES/NO for IDL
 echo 6. 24/48 hour for fcst 
 echo Example: degrib.sh 1998091600n.igb SIGMA 1 nowcast NO 00
 echo Example: degrib.sh 1998091600f.igb SIGMA 1 forecast NO 24
 echo
 echo The last arguement is for forecast cycle only .. if it is 24hr or 48 hr
 echo available z : 1,3,5,8,10,13,18,25,33,40,50,63,75,100,125,150,200
 echo ,300,400,500,600,700,800,900,1000 meters
 echo available sigma: 1 through 18
 echo
echo
 exit 1
fi
#
# From the first arguement get date, year, month and day here
#
NDATE='/nwprod/util/exec/ndate'
arg_date=`echo $1|cut -c1-10`
VERTCOORD=$2
LAYER=$3
cast=$4
suff=`echo $4 | cut -c1`
echo suff $suff
IDL=$5
FCSTHR=$6
#if [ $FCSTHR -eq 24 -o $FCSTHR -eq 00 ]; then
val_date=`${NDATE} +24 $arg_date`
#else
#val_date=`${NDATE} +48 $arg_date`
#fi
echo val_date $val_date
YMD=`echo $val_date|cut -c1-10`
YYYY=`echo $val_date|cut -c1-4`
MM=`echo $val_date|cut -c5-6`
DD=`echo $val_date|cut -c7-8`
HH=`echo $val_date|cut -c9-10`
echo YYYY MM YMD DD $YYYY $MM $YMD $DD
#
case $MM in
01) mon="January" ;;
02) mon="February" ;;
03) mon="March" ;;
04) mon="April" ;;
05) mon="May" ;;
06) mon="June" ;;
07) mon="July" ;;
08) mon="August" ;;
09) mon="September" ;;
10) mon="October" ;;
11) mon="November" ;;
12) mon="December" ;;
esac
 echo mon  $mon
#
#
# The second arguement is the field ..... WTMP,SALTY,UCUR,VCUR
# The third argement is the vertical co-ord system ..SIGMA or Z
# The fourth arg is the layer (for sigma) or level for Z
# The fifth arg is the nowcast or forecast data
#
#  Set default directories
#
WORKDIR=$WKDIR
DEGRBER='/nwprod/util/exec'
cd $WORKDIR
#
#
# For water temperature or salinity or currents
#
# For values on SIGMA coordinates
#
 if [ $VERTCOORD = SIGMA ]; then
#
 case $LAYER in
1) DEPTH='kpds6=128:kpds7=0:' ;;
2) DEPTH='kpds6=128:kpds7=1:' ;;
3) DEPTH='kpds6=128:kpds7=258:' ;;
4) DEPTH='kpds6=128:kpds7=515:' ;;
5) DEPTH='kpds6=128:kpds7=774:' ;;
6) DEPTH='kpds6=128:kpds7=1549:' ;; 
7) DEPTH='kpds6=128:kpds7=3354:' ;;
8) DEPTH='kpds6=128:kpds7=6706:' ;;
9) DEPTH='kpds6=128:kpds7=12900:' ;;
10) DEPTH='kpds6=108:kpds7=2580:' ;;
11) DEPTH='kpds6=108:kpds7=5150:' ;;
12) DEPTH='kpds6=108:kpds7=7720:' ;;
13) DEPTH='kpds6=108:kpds7=10290:' ;;
14) DEPTH='kpds6=108:kpds7=12860:' ;;
15) DEPTH='kpds6=108:kpds7=15430:' ;;
16) DEPTH='kpds6=108:kpds7=18000:' ;;
17) DEPTH='kpds6=108:kpds7=20570:' ;;
18) DEPTH='kpds6=108:kpds7=23140:' ;;
esac
app=' '
echo DEPTH $DEPTH$app
#
else
#
# For values on Z-coordinates
# Set depth to user entered depth (meters)
#
  DEPTH=kpds6=160:kpds7=$LAYER:
  app='m'
echo 'Depth=' $DEPTH
fi
#
#
#--- CHECK FOR EXISTENCE OF GRIB FILE FOR CH IN

if test -f $WORKDIR/$1; then
echo The file is present for today
else
echo Oops Input GRIB file not present
exit
fi
#
#
#
## *******************************************************************************
####
##### ATTENTION   ATTENTION      ATTENTION     ATTENTION    ATTENTION    ATTENTION
####
## ********************************************************************************
### Change here if you donot want all the variables to be de-gribbed and plotted out 
### TRUE means it will create a plot and FALSE wouldnt create
#
##for FLD in WTMP SALTY UOGRD DSLM
for FLD in  DSLM
do
case $FLD in

WTMP) FLDNAME='Water Temperature'
      if [ $LAYER = 1 ]; then
        PRD=TRUE
      else
        PRD=TRUE
      fi
      ;;
SALTY) FLDNAME='Salinity'
      if [ $LAYER = 1 -o $LAYER = 200 ]; then
        PRD=TRUE
      else
        PRD=FALSE
      fi
      ;;
UOGRD) FLDNAME='Currents'
       if [ $LAYER = 1 -o $LAYER = 200 ]; then
        PRD=TRUE
      else
        PRD=FALSE
      fi
      ;;
DSLM) FLDNAME='Water Level'
      if [ $LAYER = 1 ]; then
        PRD=TRUE
      fi
 ;;
esac
#
if [ $PRD = TRUE ]; then
# Ascii File
#
if [ $FLD = DSLM ]; then
$DEGRBER/wgrib $WORKDIR/$1 | egrep ":DSLM:" | $DEGRBER/wgrib -i -text $WORKDIR/$1 -o $YMD-$FLD$3.txt
else
if [ $FLD = UOGRD ]; then
$DEGRBER/wgrib $WORKDIR/$1 | grep $DEPTH | egrep ":UOGRD:" | $DEGRBER/wgrib -i -text $WORKDIR/$1 -o $YMD-UOGRD$3.txt
$DEGRBER/wgrib $WORKDIR/$1 | grep $DEPTH | egrep ":VOGRD:" | $DEGRBER/wgrib -i -text $WORKDIR/$1 -o $YMD-VOGRD$3.txt
else
$DEGRBER/wgrib $WORKDIR/$1 | grep $DEPTH | egrep \
":$FLD:" | $DEGRBER/wgrib -i -text $WORKDIR/$1 -o $YMD-$FLD$3.txt
fi
fi
#
#
## ********************************************************************************************
####
###### IDL SCRIPT PART STARTS HERE 
####
## ********************************************************************************************
## If you want the IDL script to run, only then this part of the script
## will be excecuted
#
### Change here if you donot want all the variables to be plotted out using IDL
### TRUE means it will cretae a plot and FLASE wouldnt create
#
#
if [ $IDL = YES ]; then
#
#
TMPTPLATE=plot-cofs_tmp.tplate
SALTPLATE=plot-cofs_sal.tplate
CURTPLATE=plot-cofs_cur.tplate
#CURTPLATE=cur.tplate
DSLMTPLATE=plot-cofs_elev.tplate
OUTIDL=$val_date-$FLD-$3$suff.ps
#
#
#
echo IDL $IDL cast $cast
IDLCODE=plot-$FLD.idl
case $cast in
forecast) vartitle="$FCSTHR"hr" Forecast of $FLDNAME at $2 = $3$app "
          assimtitle= "      "
#wpo
          if [ ${VERTCOORD} = SIGMA -a ${LAYER} -eq 1 ] 
          then
          vartitle="$FCSTHR"hr" Forecast of $FLDNAME at Surface with Gulf Stream North Wall Position"   
          fi
#wpo
          ;;
nowcast) vartitle="Nowcast of $FLDNAME at $2 = $3$app"
         assimtitle="Assimilation of SST + GFO + JASON"

#wpo
          if [ ${VERTCOORD} = SIGMA -a ${LAYER} -eq 1 ] 
          then
          vartitle="Nowcast of $FLDNAME at Surface with Gulf Stream North Wall Position"   
          fi
#wpo


         ;;
esac
mask_file=mask_$LAYER"m"
echo vartitle $vartitle 
# ------------------- Create IDL Code ---------------------------
#
echo IDLCODE $IDLCODE
echo "pro box, x0, y0, x1, y1, color" > $IDLCODE
echo "        polyfill, [x0,x0,x1,x1], [y0,y1,y1,y0], col=color,/normal" >> $IDLCODE
echo "end" >> $IDLCODE
echo ";-------------------------------------------------------------------" >> $IDLCODE
#
echo "set_plot,'ps'" >> $IDLCODE
echo "device,landscape=1" >> $IDLCODE
# POSTSCRIPT GRAPHIC FILE
echo "device,filename='$OUTIDL'"  >> $IDLCODE
#
#
# COFS  INPUT FILE
if [ $FLD = UOGRD ]; then
echo "openr,1,'$YMD-UOGRD$3.txt'" >> $IDLCODE
echo "openr,6,'$YMD-VOGRD$3.txt'" >> $IDLCODE
else
echo "openr,1,'$YMD-$FLD$3.txt'" >> $IDLCODE
fi
echo "openr,2,'/u/wx21rg/save/rofs_gsf/graphics/util/lat.2d'" >> $IDLCODE
echo "openr,3,'/u/wx21rg/save/rofs_gsf/graphics/util/lon.2d'" >> $IDLCODE
echo "openr,4,'/u/wx21rg/save/rofs_gsf/graphics/util/$mask_file'" >> $IDLCODE
echo "openr,5,'/u/wx21rg/save/rofs_gsf/graphics/util/ecofs-depths.2d'" >> $IDLCODE
#
echo "toptitle3='$vartitle'" >> $IDLCODE
echo "bottitle4='$assimtitle'" >> $IDLCODE
echo "toptitle4='Valid: 00 UTC     $YYYY $mon $DD '" >> $IDLCODE
# Add type of data used
if [ $FLD = WTMP -a $PRD = TRUE ]; then
  cat $WORKDIR/$TMPTPLATE >> $IDLCODE
elif [ $FLD = SALTY -a $PRD = TRUE ]; then
  cat $WORKDIR/$SALTPLATE >> $IDLCODE
elif [ $FLD = UOGRD -a $PRD = TRUE ]; then
  cat $WORKDIR/$CURTPLATE >> $IDLCODE
elif [ $FLD = DSLM -a $PRD = TRUE ]; then
  cat $WORKDIR/$DSLMTPLATE >> $IDLCODE
fi 
## prepare to execute IDL code
echo ".run $WORKDIR/$IDLCODE" > idlfile3
echo "exit" >> idlfile3
#
## ------------------------------------- RUN IDL --------------------------------
# Use demo version of IDL 4.0.1 on SGI77
# use.demo
# added the commands instead of using the alias use.demo
#setenv IDL_DIR "/opt/rsi/idl"
#path = $path /opt/rsi/idl/bin 
#
#
# Run IDL program
  /usrx/local/rsi/idl_6.1/bin/idl < $WORKDIR/idlfile3
else
  exit
fi
## Print commands here
#
#if [ $LAYER = 1 ]; then
#  lpr -Pphaser4 $WORKDIR/$OUTIDL
#elif [ $LAYER = 100 -a $FLD = WTMP ]; then
# lpr -Pphaser4 $WORKDIR/$OUTIDL
#elif [ $LAYER = 200 -a $FLD = WTMP ]; then
#  lpr -Pphaser4 $WORKDIR/$OUTIDL
#wpo
#elif [ $LAYER =  50 -a $FLD = WTMP ]; then
# lpr -Pphaser4 $WORKDIR/$OUTIDL
#elif [ $LAYER = 150 -a $FLD = WTMP ]; then
# lpr -Pphaser4 $WORKDIR/$OUTIDL
#elif [ $LAYER = 200 -a $FLD = UOGRD ]; then
#  lpr -Pphaser4 $WORKDIR/$OUTIDL
#wpo
#
#fi
fi
done
