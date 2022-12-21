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

while ($line = <STDIN> ) {
  @words = split(/\s+/,$line);

  if (! ($words[$gr22] > $GR22LIM || $words[$gr37] > $GR37LIM) && 
      ($words[$gr89] > $GR89LIM) ) {
    if ($words[4] > 0 && $words[4] < 128) {
      printf(" %5.2f %3d  %9.6f %9.6f %9.6f\n",$words[3], $words[4], 
                $words[$gr22], $words[$gr37], $words[$gr89]);
    }
  }
}
