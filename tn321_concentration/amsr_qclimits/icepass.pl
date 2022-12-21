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

  if (! ($words[$gr22] > $GR22LIM || $words[$gr37] > $GR37LIM || 
        ($words[$gr89] > $GR89LIM) ) ) {
    if ($words[4] > 0 && $words[4] < 128) {
      $k = 0;
      for ($j = 5; $j <= 18; $j++) {
        if ($words[$j] > 273.15) {
           $k += 1;
        }
      }
      if ($k > 0) {
        print $line;
      }
    }
  }
}
