#!/bin/sh
#Robert Grumbine
#19  May 2016

#set -x
# ---- Setting up the universe -- ironically, should need no changes ---
# -- more variable parts are lower down -----------------------------
#rg side of things to do pseudo-operational sea ice model
source ~/.bash_profile
export basecom=${basecom:-/u/Robert.Grumbine/noscrub}
export com=${com:-$basecom/com/}
export SCRIPT_DIR=/u/Robert.Grumbine/desk/model/skill/scripts

export tag=${tag:-`date +"%Y%m%d"`}

export PDY=$tag
export PDYm1=`expr $PDY - 1` ; PDYm1=`dtgfix3 $PDYm1`
export PDYm2=`expr $PDYm1 - 1`; PDYm2=`dtgfix3 $PDYm2`
export PDYm3=`expr $PDYm2 - 1`; PDYm3=`dtgfix3 $PDYm3`
export PDYm4=`expr $PDYm3 - 1`; PDYm4=`dtgfix3 $PDYm4`
export PDYm5=`expr $PDYm4 - 1`; PDYm5=`dtgfix3 $PDYm5`
export PDYm6=`expr $PDYm5 - 1`; PDYm6=`dtgfix3 $PDYm6`
export PDYm7=`expr $PDYm6 - 1`; PDYm7=`dtgfix3 $PDYm7`
export PDYm8=`expr $PDYm7 - 1`; PDYm8=`dtgfix3 $PDYm8`
export PDYm9=`expr $PDYm8 - 1`; PDYm9=`dtgfix3 $PDYm9`
export PDYm10=`expr $PDYm9 - 1`; PDYm10=`dtgfix3 $PDYm10`
export PDYm11=`expr $PDYm10 - 1`; PDYm11=`dtgfix3 $PDYm11`
export PDYm12=`expr $PDYm11 - 1`; PDYm12=`dtgfix3 $PDYm12`
export PDYm13=`expr $PDYm12 - 1`; PDYm13=`dtgfix3 $PDYm13`
export PDYm14=`expr $PDYm13 - 1`; PDYm14=`dtgfix3 $PDYm14`
export PDYm15=`expr $PDYm14 - 1`; PDYm15=`dtgfix3 $PDYm15`
export PDYm16=`expr $PDYm15 - 1`; PDYm16=`dtgfix3 $PDYm16`

echo Working on $tag $PDYm16
RUNDIR=/ptmpd2/rg$$
mkdir -p $RUNDIR
cd $RUNDIR

# ---------------------- Setting up the model run(s) ------------
#    main structure should need no changes
set -x
#
export model=persist
export version=v0.0.0
${SCRIPT_DIR}/persistscore.sh

export model=kiss
export version=v1.0.1
${SCRIPT_DIR}/icescore.sh
#
export model=gfs
export version=v1.0.0
${SCRIPT_DIR}/gfsscore.sh
#
export model=cfs
export version=v2.0.0
${SCRIPT_DIR}/cfsscore.sh
#
export model=acnfs
export version=v0.0.0
${SCRIPT_DIR}/acnfsscore.sh

exit
# ----------------------------------------------------------------
# Now that we have all the models scored, construct some simple plots
# By measure, model vs. forecast lead
#cp /usr1/rmg3/model/skill/scripts/gnuplot_in .
cp ${SCRIPT_DIR}/gnuplot_in .
gnuplot gnuplot_in
chmod a+r *.png
scp -p *.png seaice@emcrzdm:/home/www/polar/develop/icemodel/
