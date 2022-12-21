#!/usr/bin/perl

$m1 = 5.0;
$m2 = 1.6*1.25;
$l1 = 0.35;
$l2 = 0.65;
$g  = 9.8;

$pi = 3.141592654;
$theta = -$pi/2./6.;
$dtheta = 0.000;
$dt     = 1./100.;

$phi = 3.; #artificial amplification of gravity/force.  This value gives
           #approx 2/3 second period.

for ($i = 0; $i < 500; $i++) {
  $lhs = ($m1 / 3. + $m2)*$l1*$l1 + $m2 * $l2 *$l2 / 3.;
  $rhs = $l1 * sin($theta)*($m1/2 + $m2) - $l2*$m2*cos($theta)/2. ;
  $dtheta -= (1.+$phi)*$g * $rhs / $lhs * $dt;
  $theta  += $dtheta * $dt;
  printf "%f %f %3d %6.3f  %6.3f\n",$lhs, $rhs, $i, $theta*180./$pi, $dtheta;
}

