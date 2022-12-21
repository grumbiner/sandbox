#!/usr/bin/perl
#Robert Grumbine
#29 Apr 2009

$t19v = 9;
$t19h = 10;
$t22v = 11;
$t37v = 13;
$t37h = 14;
$t89v = 15;
$t89h = 16;
#$t89bv = 17;

while ($line = <STDIN> ) {
  @words = split(/\s+/,$line);
  if ($words[$t19v] < $words[$t19h]) {
    $stokes = sqrt( ($words[$t19h] * $words[$t19h]) - ($words[$t19v] * $words[$t19v]) );
    chop($line);

    printf("%3d  %f \n",$words[4],$stokes);

  }
}
