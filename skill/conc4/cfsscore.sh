#!/bin/sh

#set -x

export basecom=${basecom:-/u/Robert.Grumbine/noscrub}
export nwprod=${nwprod:-/u/Robert.Grumbine/desk/model}

export com=${com:-$basecom/com/}


export model=${model:-cfs}
export version=${version:-v2.0.0}
export remote=seaice@emcrzdm

export exec=$nwprod/skill/exec
export obs=${com}/mmab/prod/sice
export fcst=${com}/mmab/developer/${model}_${version}

#-------------------------------------------------------------------------------
wgrib $obs.$PDY/seaice.t00z.5min.grb | wgrib -i -nh $obs.$PDY/seaice.t00z.5min.grb -o obsd

echo Valid day $PDY > scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm1/cfs_ice.bin ${nwprod}/skill/fix/skip 4  >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm2/cfs_ice.bin ${nwprod}/skill/fix/skip 8  >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm3/cfs_ice.bin ${nwprod}/skill/fix/skip 12 >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm4/cfs_ice.bin ${nwprod}/skill/fix/skip 16 >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm5/cfs_ice.bin ${nwprod}/skill/fix/skip 20 >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm6/cfs_ice.bin ${nwprod}/skill/fix/skip 24 >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm7/cfs_ice.bin ${nwprod}/skill/fix/skip 28 >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm8/cfs_ice.bin ${nwprod}/skill/fix/skip 32 >> scoreout.$PDY

cp scoreout.$PDY $fcst.$PDY
scp scoreout.$PDY ${remote}:/home/www/polar/develop/icemodel/${model}_${version}/
scp scoreout.$PDY ${remote}:/home/www/polar/develop/icemodel/scoreout.${model}_${version}
