#!/usr/bin/perl
#Robert Grumbine
#29 Apr 2009

$t19v = 8;
$t22v = 10;
$t37v = 12;
$t89v = 14;
#$t89bv = 16;

while ($line = <STDIN> ) {
  @words = split(/\s+/,$line);
  $gr22 = ($words[$t22v] - $words[$t19v]) / ($words[$t22v] + $words[$t19v]);
  $gr37 = ($words[$t37v] - $words[$t19v]) / ($words[$t37v] + $words[$t19v]);
  $gr89 = ($words[$t89v] - $words[$t19v]) / ($words[$t89v] + $words[$t19v]);
  chop($line);
  print $line;
  printf("  %9.6f %9.6f %9.6f\n",$gr22, $gr37, $gr89);
}
  
#ice  75.31 -117.87   99   229.4 250.8 233.4 248.0 234.8 249.9 232.9 249.1 232.2 246.8 222.7 231.8 217.7 226.8
