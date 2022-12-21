#!/usr/bin/perl

#Robert Grumbine
#27 January 2016
#
#Collect the analysis ideas from the 2009-2010 qclimits work
#
$t6v  = 7
$t7v  = 12
$t10v = 17
$t19h = 21;
$t19v = 22;
$t24v = 27;
$t37v = 32;

$lat = 1;
$lon = 2;
$icecon = 3;
$sst    = 4;


$GR22LIM = 0.045; # NT2
$GR37LIM = 0.050; # NT2
$GR89LIM = 0.150; # RG

while ($line = <STDIN> ) {
  @words = split(/\s+/,$line);

  chop($line);
  print $line;

  $gr22 = ($words[$t22v] - $words[$t19v]) / ($words[$t22v] + $words[$t19v]);
  $gr37 = ($words[$t37v] - $words[$t19v]) / ($words[$t37v] + $words[$t19v]);
  printf("  %9.6f %9.6f \n",$gr22, $gr37);

  $stokes = sqrt( ($words[$t19h] * $words[$t19h]) - ($words[$t19v] * $words[$t19v]) );
  printf("%3d  %f \n",$words[$icecon],$stokes);

}
