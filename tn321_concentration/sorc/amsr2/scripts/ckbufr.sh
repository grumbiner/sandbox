#
set -xeu
##
WORKHOME=/gpfs/gd1/emc/marine/save/Vera.Gerald/amsre/amsr2
WORKTMP=/stmp/Vera.Gerald/amsr2.sh
mkdir -p $WORKTMP
cd $WORKTMP
#
DUMP=/nwprod/ush/dumpjb
export XLFRTEOPTS="unit_vars=yes"
##export TANK=/dcom/us007003
export TANK=/ptmp/Vera.Gerald/amsr2_btmp
##
HR="00"
cp /com/date/t${HR}z  date
PDY=`cut -c7-14 date`
echo "PDY = $PDY"
##PDY=2004102600
##
#####    DUMP SSMI reprocessed products data  #########
##
cp $TANK/AMSR2-MBT-89B_v1r0_GW1_s201303112350597_e201303112359582_c201310241559370.bufr   amr2btmp
cp $TANK/AMSR2_1B_BUFR_Table   bufr.tab
#
##   ssmi input
export FORT11="amr2btmp"
export FORT12=" bufr.tab"
#    output 
export FORT51="amr2btmp.${PDY}"
###
 $WORKHOME/amsr2.x 1> ft06 2> errfile
###
