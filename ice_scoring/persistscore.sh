#!/bin/sh
#Robert Grumbine
#19 May 2016

#set -x

export base=${base:-/u/Robert.Grumbine/noscrub}
export com=${com:-$base/com/}

export nwprod=${nwprod:-/u/Robert.Grumbine/desk/model}
export exec=$nwprod/skill/exec

export model=${model:-persist}
export version=${version:-v0.0.0}
export obs=${com}/mmab/prod/sice
export fcst=${com}/mmab/developer/${model}_${version}.$PDY
if [ ! -d $fcst ] ; then
  mkdir -p $fcst
fi

for d in $PDY $PDYm1 $PDYm2 $PDYm3 $PDYm4 $PDYm5 $PDYm6 $PDYm7 $PDYm8 $PDYm9 $PDYm10 $PDYm11 $PDYm12 $PDYm13 $PDYm14 $PDYm15 $PDYm16
do
  wgrib $obs.$d/seaice.t00z.5min.grb | wgrib -i -nh $obs.$d/seaice.t00z.5min.grb -o obsd.$d
done

echo Valid day $PDY bias rms pod far fcr correct > scoreout.$PDY
$exec/${model}_score obsd.$PDY obsd.$PDYm1 ${nwprod}/skill/fix/skip 1  >> scoreout.$PDY
$exec/${model}_score obsd.$PDY obsd.$PDYm2 ${nwprod}/skill/fix/skip 2  >> scoreout.$PDY
$exec/${model}_score obsd.$PDY obsd.$PDYm3 ${nwprod}/skill/fix/skip 3  >> scoreout.$PDY
$exec/${model}_score obsd.$PDY obsd.$PDYm4 ${nwprod}/skill/fix/skip 4  >> scoreout.$PDY
$exec/${model}_score obsd.$PDY obsd.$PDYm5 ${nwprod}/skill/fix/skip 5  >> scoreout.$PDY
$exec/${model}_score obsd.$PDY obsd.$PDYm6 ${nwprod}/skill/fix/skip 6  >> scoreout.$PDY
$exec/${model}_score obsd.$PDY obsd.$PDYm7 ${nwprod}/skill/fix/skip 7  >> scoreout.$PDY
$exec/${model}_score obsd.$PDY obsd.$PDYm8 ${nwprod}/skill/fix/skip 8  >> scoreout.$PDY
$exec/${model}_score obsd.$PDY obsd.$PDYm9 ${nwprod}/skill/fix/skip 9  >> scoreout.$PDY
$exec/${model}_score obsd.$PDY obsd.$PDYm10 ${nwprod}/skill/fix/skip 10  >> scoreout.$PDY
$exec/${model}_score obsd.$PDY obsd.$PDYm11 ${nwprod}/skill/fix/skip 11  >> scoreout.$PDY
$exec/${model}_score obsd.$PDY obsd.$PDYm12 ${nwprod}/skill/fix/skip 12  >> scoreout.$PDY
$exec/${model}_score obsd.$PDY obsd.$PDYm13 ${nwprod}/skill/fix/skip 13  >> scoreout.$PDY
$exec/${model}_score obsd.$PDY obsd.$PDYm14 ${nwprod}/skill/fix/skip 14  >> scoreout.$PDY
$exec/${model}_score obsd.$PDY obsd.$PDYm15 ${nwprod}/skill/fix/skip 15  >> scoreout.$PDY
$exec/${model}_score obsd.$PDY obsd.$PDYm16 ${nwprod}/skill/fix/skip 16  >> scoreout.$PDY

cp scoreout.$PDY $fcst
chmod 644 scoreout.$PDY
scp scoreout.$PDY seaice@emcrzdm:/home/www/polar/develop/icemodel/${model}_${version}/
scp scoreout.$PDY seaice@emcrzdm:/home/www/polar/develop/icemodel/scoreout.${model}_${version}
