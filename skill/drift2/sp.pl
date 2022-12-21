#!/usr/bin/perl

while ($line=<STDIN>) {
  chop($line);
  @stats=split(/ +/,$line);
  $rate_bias = $stats[9] / $stats[3];
  #print $stats[3],"\n";
  if ($rate_bias > 30.0 || $rate_bias < -30.0) {
    print $line;
    printf "%7.1f\n",$rate_bias;
  }
}
