#!/usr/bin/perl

$scale = 100;
$dist_toler = 20.0;

while ($line = <STDIN>) {
  chop($line);
  @words = split(/\s+/, $line);
  $n = $#words;
  if ($words[$n] < $dist_toler) {
    $count{ int($scale*$words[$n-1]+0.5) } += 1;
  }
}
while ( ($key,$value) = each(%count) ) {
  printf("%5.2f %5d\n",$key/$scale, $value);
}

