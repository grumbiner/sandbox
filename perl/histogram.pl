#!/usr/bin/perl

$scale = 100;

while ($line = <STDIN>) {
  chop($line);
  @words = split(/\s+/, $line);
  $n = $#words;
  $count{ int($scale*$words[$n-1]+0.5) } += 1;
}
while ( ($key,$value) = each(%count) ) {
  printf("%5.2f %5d\n",$key/$scale, $value);
}

