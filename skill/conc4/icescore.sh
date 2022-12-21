#!/bin/sh

set -x

export base=${base:-/u/Robert.Grumbine/noscrub}
export nwprod=${nwprod:-/u/Robert.Grumbine/desk/model}

export com=${com:-$base/com/}
export model=${model:-kiss}
export version=${version:-v1.0.1}

export exec=${nwprod}/skill/exec
export obs=${com}/mmab/prod/sice
export fcst=${com}/mmab/developer/${model}_${version}

wgrib $obs.$PDY/seaice.t00z.5min.grb | wgrib -i -nh $obs.$PDY/seaice.t00z.5min.grb -o obsd

echo Valid day $PDY > scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm1/running.out.1 ${nwprod}/${model}_${version}/fix/skip  4 0 >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm2/running.out.1 ${nwprod}/${model}_${version}/fix/skip  8 0 >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm3/running.out.1 ${nwprod}/${model}_${version}/fix/skip 12 0 >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm4/running.out.2 ${nwprod}/${model}_${version}/fix/skip  4 3 >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm5/running.out.2 ${nwprod}/${model}_${version}/fix/skip  8 3 >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm6/running.out.2 ${nwprod}/${model}_${version}/fix/skip 12 3 >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm7/running.out.3 ${nwprod}/${model}_${version}/fix/skip  4 6 >> scoreout.$PDY
$exec/${model}_score obsd $fcst.$PDYm8/running.out.3 ${nwprod}/${model}_${version}/fix/skip  8 6 >> scoreout.$PDY

cp scoreout.$PDY $fcst.$PDY
chmod 644 scoreout.$PDY
scp scoreout.$PDY seaice@emcrzdm:/home/www/polar/develop/icemodel/${model}_${version}/
scp scoreout.$PDY seaice@emcrzdm:/home/www/polar/develop/icemodel/scoreout.${model}_${version}

