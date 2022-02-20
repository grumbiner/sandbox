#
set -xeu
##
WORKHOME=/gpfs/gd1/emc/marine/save/Vera.Gerald/amsre/amsr2
WORKTMP=/stmp/Vera.Gerald/amsr2hr.sh
mkdir -p $WORKTMP
cd $WORKTMP
#
DUMP=/nwprod/ush/dumpjb
export XLFRTEOPTS="unit_vars=yes"
##export TANK=/dcom/us007003
export TANK=/gpfs/gd1/emc/marine/noscrub/Vera.Gerald/amsr2_btmp
##
HR="00"
cp /com/date/t${HR}z  date
PDY=`cut -c7-14 date`
echo "PDY = $PDY"
##PDY=2004102600
##
#####    DUMP AMSR2 89A or 89B products data  #########
##
cp $TANK/AMSR2-MBT-89A_v1r0_GW1_s201303112323599_e201303112332583_c201310241558310.bufr   amsr2_hr.bufr
cp $TANK/AMSR2_1B_BUFR_Table   bufr.tab
#
##   ssmi input
export FORT11="amsr2_hr.bufr"
export FORT12=" bufr.tab"
#    output 
export FORT51="amsr2hr.outp"
###
 $WORKHOME/amsr2hr.x 1> ft06 2> errfile
###
