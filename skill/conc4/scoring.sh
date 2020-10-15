#!/bin/sh

tag=20130205
export PDY=${tag:-`date +"%Y%m%d"`}
export com=${com:-/u/Robert.Grumbine/noscrub}
export nwprod=${nwprod:-/u/Robert.Grumbine/desk/model}

export FIX=${nwprod}/statistical/fix
export EXEC=${nwprod}/statistical/exec
export COMINbase=${com}/com/mmab/prod/
export FCSTbase=${com}/com/mmab/developer/

export BASEDIR=/usr2/rmg3/wx21rg
export BASEDIR=/ptmpd2/wx21rg/stats

PATH=$PATH:/usr/local/grads/bin:.

#----------------------------------------------------
job=statverf
pid=$$
export DATA=$BASEDIR/${job}.$pid
mkdir -p $DATA
cd $DATA

set -xe

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
if [ $PDYm16 -lt 20130120 ] ; then
  echo need move date forward more, so that m16 is 20130120, currently $PDYm16
  exit
fi

#-----------------------------------------------------------------------------

#PDY = verification day, PDYm## = forecast date
cp $FIX/skip_hr skip

while [ $PDY -le 20130227 ]
do
wgrib ${COMINbase}/sice.$PDY/seaice.t00z.5min.grb | wgrib -i -nh ${COMINbase}/sice.$PDY/seaice.t00z.5min.grb -o conc.$PDY

time  $EXEC/score skip     conc.$PDY           $FCSTbase/stats.$PDY/fcst.$PDY \
            $FCSTbase/stats.$PDYm1/fcst.$PDYm1 $FCSTbase/stats.$PDYm2/fcst.$PDYm2 \
            $FCSTbase/stats.$PDYm3/fcst.$PDYm3 $FCSTbase/stats.$PDYm4/fcst.$PDYm4 \
            $FCSTbase/stats.$PDYm5/fcst.$PDYm5 $FCSTbase/stats.$PDYm6/fcst.$PDYm6 \
            $FCSTbase/stats.$PDYm7/fcst.$PDYm7 $FCSTbase/stats.$PDYm8/fcst.$PDYm8 \
            $FCSTbase/stats.$PDYm9/fcst.$PDYm9 $FCSTbase/stats.$PDYm10/fcst.$PDYm10 \
            $FCSTbase/stats.$PDYm11/fcst.$PDYm11 $FCSTbase/stats.$PDYm12/fcst.$PDYm12 \
            $FCSTbase/stats.$PDYm13/fcst.$PDYm13 $FCSTbase/stats.$PDYm14/fcst.$PDYm14 \
            $FCSTbase/stats.$PDYm15/fcst.$PDYm15 $FCSTbase/stats.$PDYm16/fcst.$PDYm16                
#gprof $EXEC/score > prof.$PDY

#will create or assume created files for lead 0-16, by name, by a+ in fopen,
#  in writing the program.

  PDYm16=$PDYm15
  PDYm15=$PDYm14
  PDYm14=$PDYm13
  PDYm13=$PDYm12
  PDYm12=$PDYm11
  PDYm11=$PDYm10
  PDYm10=$PDYm9
  PDYm9=$PDYm8
  PDYm8=$PDYm7
  PDYm7=$PDYm6
  PDYm6=$PDYm5
  PDYm5=$PDYm4
  PDYm4=$PDYm3
  PDYm3=$PDYm2
  PDYm2=$PDYm1
  PDYm1=$PDY
  
  PDY=`expr $PDY + 1`; PDY=`dtgfix3 $PDY`

done
