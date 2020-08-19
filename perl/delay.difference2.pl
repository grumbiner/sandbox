#!/usr/bin/perl

# Play around with arguments and with delay-difference equations
$spinup = 32;
$lenmult = 160;
$aweight = 0.46;
$sweight = 0.68;
$alag =  6;
$slag = 16;
$asweight = 0.40;
$saweight = 0.45;
$magrand = 0.3;


for ($n = 0; $n < $spinup; $n++) {
  $soi[$n] = sin(2.*3.141592654*$n / 16);
  $aa[$n]  = sin(2.*3.141592654*$n / 16);   
  printf "%6.3f %6.3f\n", $soi[$n],$aa[$n];
}

for ($n = $spinup; $n < $lenmult*$spinup; $n++) {
  $soi[$n] = $soi[$n - 1] *$sweight - $aa[$n -$alag] * $asweight + $magrand * rand;
  $aa[$n]  = $aa[$n - 1] * $aweight + $soi[$n -$slag] * $saweight + $magrand * rand;
  printf "%6.3f %6.3f\n", $soi[$n],$aa[$n];
}

