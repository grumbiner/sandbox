#
set -xeu
##
WORKHOME=/gpfs/gd1/emc/marine/save/Vera.Gerald/amsre/amsr2
WORKTMP=/stmp/Vera.Gerald/amsr2lr.sh
mkdir -p $WORKTMP
cd $WORKTMP
#
DUMP=/nwprod/ush/dumpjb
export XLFRTEOPTS="unit_vars=yes"
##export TANK=/dcom/us007003
export TANK=/gpfs/gd1/emc/marine/noscrub/Vera.Gerald/amsr2_btmp
##
HR="00"
##cp /com/date/t${HR}z  date
##PDY=`cut -c7-14 date`
##echo "PDY = $PDY"
##PDY=2004102600
##
#####    DUMP SSMI reprocessed products data  #########
##
cp $TANK/AMSR2-MBT-LR_v1r0_GW1_s201303112359597_e201303120008581_c201312170355220.bufr amsr2_lr.bufr
cp $TANK/AMSR2_1B_BUFR_Table   bufr.tab
#
##   input
export FORT11="amsr2_lr.bufr"
export FORT12=" bufr.tab"
#    output 
export FORT51="amsr2lr.outp"
###
 $WORKHOME/amsr2lr.x 1> ft06 2> errfile
###
