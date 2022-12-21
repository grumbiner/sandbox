#
set -xeu
##
WORKHOME=/gpfs/gd1/emc/marine/save/Vera.Gerald/amsre/amsr2
WORKTMP=/stmp/Vera.Gerald/debufr.sh
mkdir -p $WORKTMP
cd $WORKTMP
#
debufr=/nwprod/util/exec/debufr
export XLFRTEOPTS="unit_vars=yes"
##export TANK=/dcom/us007003
export TANK=/gpfs/gd1/emc/marine/noscrub/Vera.Gerald/amsr2_btmp
##
HR="00"
cp /com/date/t${HR}z  date
PDY=`cut -c7-14 date`
echo "PDY = $PDY"
##
#####    DUMP SSMI reprocessed products data  #########
##
##cp $TANK/AMSR2-MBT-89A_v1r0_GW1_s201303112323599_e201303112332583_c201310241558310.bufr . 
cp $TANK/AMSR2-MBT-LR_v1r0_GW1_s201303112359597_e201303120008581_c201312170355220.bufr input_bufr
cp $TANK/AMSR2_1B_BUFR_Table .
#
$debufr  -t$WORKTMP -fAMSR2_1B_BUFR_Table input_bufr
#

##
