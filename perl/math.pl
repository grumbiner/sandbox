#!/usr/bin/perl
use Math;

while ($line = <STDIN>) {
  @words=split(/\s+/,$line);
  print $words[1],"\n";
#  $x = int($words[1]+0.5);
#  if ($x < 0) $x += 1;
#  $tally{$x} += 1;
}
while ( ($key,$value) = each(%tally) ) {
  print $value," ",$key,"\n";
}
