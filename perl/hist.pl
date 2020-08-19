#!/usr/bin/perl

$n = $ARGV[0];
#print $n,"\n";

while ($line = <STDIN>) {
  chop($line);
  @words = split(/\s+/, $line);
  $count{$words[$n]} += 1;
}
while ( ($key,$value) = each(%count) ) {
  print $key," ",$value,"\n";
}
