#!/usr/bin/perl

$n = $ARGV[0];
$scale = 10.0;
#print $n,"\n";

while ($line = <STDIN>) {
  chop($line);
  @words = split(/\s+/, $line);
  $count{ int($scale*$words[$#words]+0.5) } += 1;
}
while ( ($key,$value) = each(%count) ) {
  printf("%5.2f %5d\n",$key/$scale, $value);
}
