#!/usr/bin/perl

$bot = $ARGV[0]/100;
$top = $ARGV[1]/100;
$leg = 3*$ARGV[2];

for ($i = 0; $i <= 13; $i++) {
  $sum[$i] = 0;
  $sumsq[$i] = 0;
}
$count += 1;

while ($line = <STDIN> ) {
  chop($line);
  @words=split(/\s+/, $line);
  $n = $#words;

  if ($n >= 12) {   
    # Construct statistics
    if ($words[$leg] >= $bot && $words[$leg] <= $top) {
      print $line,"\n";
      for ($i = 0; $i <= $n; $i++) {
         $sum[$i] += $words[$i];
         $sumsq[$i] += $words[$i]*$words[$i];
      }
      $tmp = $words[$n]*10;
      $cat = $tmp - ($tmp % 100);
      $cat /= 100;
      $histo{ $cat } += 1;
      $count += 1;
    }
  }
}

