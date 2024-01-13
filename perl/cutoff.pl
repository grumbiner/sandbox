#!/usr/bin/perl

$lim = $ARGV[0];

while ($line = <STDIN>) {
  #chop($line);
  @words = split(/\s+/, $line);
  #$count{ int($scale*$words[$n]+0.5) } += 1;
  $n = $#words;
  if (abs($words[$n]) > $lim) {
    print $line;
  }
}
while ( ($key,$value) = each(%count) ) {
  printf("%5.2f %5d\n",$key/$scale, $value);
}
