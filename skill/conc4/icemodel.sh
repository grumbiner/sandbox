#!/bin/sh

PATH=${PATH}:/usr/local/bin

# ---- Setting up the universe -- ironically, should need no changes ---
# -- more variable parts are lower down -----------------------------
#rg side of things to do pseudo-operational sea ice model
source ~/.bash_profile

export basecom=${basecom:-/u/Robert.Grumbine/noscrub}
export basenwprod=${basenwprod:-/u/Robert.Grumbine/desk}

export com=$basecom/com
export nwprod=$basenwprod/model/

export tag=${tag:-`date +"%Y%m%d"`}
#export tag=20121129

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

# Update the 'production' directories:
$nwprod/gfs/prod/gdas.sh
$nwprod/gfs/prod/gfs.sh

# ---------------------- Setting up the model run(s) ------------
#    main structure should need no changes
export model=kiss
export version=v0.0.0

# Run the sea ice model:
${nwprod}/${model}_${version}/sms/sms.fake

# Distribute the output, score the runs, ...
${nwprod}/${model}_${version}/auxiliary/alphamodelweb.sh

#Do scoring separately
#-------------------------------------------------------
