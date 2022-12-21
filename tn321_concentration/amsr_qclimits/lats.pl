#!/usr/bin/perl
#Robert Grumbine
#29 Apr 2009

$t19v = 9;
$t22v = 11;
$t37v = 13;
$t89v = 15;
$gr22 = 19;
$gr37 = 20;
$gr89 = 21;

$GR22LIM = 0.045; # NT2
$GR37LIM = 0.050; # NT2
$GR89LIM = 0.150; # RG
$lat = 1;
$lon = 2;

while ($line = <STDIN> ) {
  @words = split(/\s+/,$line);

  if ($words[$lat] > 80 || $words[$lat] < -80) {
    print $line;
  }

}
