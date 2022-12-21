#!/usr/bin/perl

while ($line=<STDIN>) {
  chop($line);
  @stats=split(/ +/,$line);
  $rate = $stats[10] / $stats[4];
  #print $stats[4],"\n";
  #if ($rate > 30.0 ) {
    print $line;
    printf "%7.1f\n",$rate;
  #}
}
