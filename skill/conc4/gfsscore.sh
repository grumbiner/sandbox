#!/bin/sh

#set -x

export base=${base:-/u/Robert.Grumbine/noscrub}
export com=${com:-$base/com/}

export nwprod=${nwprod:-/u/Robert.Grumbine/desk/model}
export exec=$nwprod/skill/exec

export model=${model:-gfs}
export version=${version:-v1.0.0}
export obs=${com}/mmab/prod/sice
export fcst=${com}/mmab/developer/${model}_${version}

wgrib $obs.$PDY/seaice.t00z.5min.grb | wgrib -i -nh $obs.$PDY/seaice.t00z.5min.grb -o obsd

echo Valid day $PDY > scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm1/gfs_ice.bin ${nwprod}/skill/fix/skip 4  >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm2/gfs_ice.bin ${nwprod}/skill/fix/skip 8  >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm3/gfs_ice.bin ${nwprod}/skill/fix/skip 12 >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm4/gfs_ice.bin ${nwprod}/skill/fix/skip 16 >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm5/gfs_ice.bin ${nwprod}/skill/fix/skip 20 >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm6/gfs_ice.bin ${nwprod}/skill/fix/skip 24 >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm7/gfs_ice.bin ${nwprod}/skill/fix/skip 28 >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm8/gfs_ice.bin ${nwprod}/skill/fix/skip 32 >> scoreout.$PDY

cp scoreout.$PDY $fcst.$PDY
chmod 644 scoreout.$PDY
scp scoreout.$PDY seaice@emcrzdm:/home/www/polar/develop/icemodel/${model}_${version}/
scp scoreout.$PDY seaice@emcrzdm:/home/www/polar/develop/icemodel/scoreout.${model}_${version}

