#!/usr/bin/perl

$maxsp = 45.0;
$minsp = 0.25;

while ($line=<STDIN>) {
  chop($line);
  @stats=split(/ +/,$line);
  if ($stats[12] > 0) {
  $rate = $stats[10] / $stats[12];
  if ($rate < $maxsp && $rate > $minsp ) {
    print $line,"\n";
  }
  }
}
