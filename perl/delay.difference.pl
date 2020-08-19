#!/usr/bin/perl

# Play around with arguments and with delay-difference equations
$spinup = 32;
#$aweight = 0.46;
#$sweight = 0.68;
$aweight = 0.23;
$sweight = 0.23;
$alag =  6;
$slag = 16;
$asweight = 0.55;
$saweight = 0.35;


for ($n = 0; $n < $spinup; $n++) {
  $soi[$n] = sin(2.*3.141592654*$n / 16);
  $aa[$n]  = sin(2.*3.141592654*$n / 16);   
  printf "%6.3f %6.3f\n", $soi[$n],$aa[$n];
}

for ($n = $spinup; $n < 10*$spinup; $n++) {
  $soi[$n] = $soi[$n - 1] *$sweight - $aa[$n -$alag] * $asweight;
  $aa[$n]  = $aa[$n - 1] * $aweight + $soi[$n -$slag] * $saweight;
  printf "%6.3f %6.3f\n", $soi[$n],$aa[$n];
}

