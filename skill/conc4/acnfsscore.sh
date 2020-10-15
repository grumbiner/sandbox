#!/bin/sh

#set -x
echo now trying to do the acnfs model scoring
export basecom=${basecom:-/u/Robert.Grumbine/noscrub}
export com=${com:-$basecom/com}

export basenwprod=${basenwprod:-/u/Robert.Grumbine/desk}
export nwprod=${nwprod:-$basenwprod/model}

export model=${model:-acnfs}
export version=${version:-v0.0.0}

export exec=${nwprod}/skill/exec/
export obs=${com}/mmab/prod/sice
export fcst=${com}/mmab/developer/${model}_${version}

wgrib $obs.$PDY/seaice.t00z.5min.grb | wgrib -i -nh $obs.$PDY/seaice.t00z.5min.grb -o obsd

echo Valid day $PDY > scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm1/graphics ${nwprod}/skill/fix/skip 1 >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm2/graphics ${nwprod}/skill/fix/skip 2 >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm3/graphics ${nwprod}/skill/fix/skip 3 >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm4/graphics ${nwprod}/skill/fix/skip 4 >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm5/graphics ${nwprod}/skill/fix/skip 5 >> scoreout.$PDY
#$exec/${model}_score obsd $fcst.$PDYm6/graphics ${nwprod}/skill/fix/skip 6 >> scoreout.$PDY
#$exec/${model}_score obsd $fcst.$PDYm7/graphics ${nwprod}/skill/fix/skip 7 >> scoreout.$PDY

cp scoreout.$PDY $fcst.$PDY
chmod 644 scoreout.$PDY
scp -p scoreout.$PDY seaice@emcrzdm:/home/www/polar/develop/icemodel/${model}_${version}/
scp -p scoreout.$PDY seaice@emcrzdm:/home/www/polar/develop/icemodel/scoreout.${model}_${version}
