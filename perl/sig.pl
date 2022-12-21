#!/usr/bin/perl

for ($n = 12; $n <= 216; $n += 12) {
  for ($rho = 0.1; $rho <= 0.8; $rho += 0.01) {
     $y = $rho * sqrt($n - 2) / sqrt(1.-$rho*$rho);
     #print $n," ",$rho," ",$y,"\n";
     printf "%3d %4.2f %5.2f\n", $n,$rho,$y;
  }
} 
