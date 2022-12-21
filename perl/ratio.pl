#!/usr/bin/perl

#$t1 = $ARGV[0];
#$t2 = $ARGV[1];

$av1 = 0.;
$av2 = 0.;
$avratio = 0.;
$count = 0;

while ( $line = <STDIN> ) {
  chop($line);
  @times = split(" ",$line);
  $s1 = &tosecs($times[0]);
  $s2 = &tosecs($times[1]);
  $count += 1;
  $av1 += $s1;
  $av2 += $s2;
  $avratio +=  $s2/$s1/1.60934;
  printf "%5.0f %5.0f %5.3f %5.3f\n",$s1, $s2, $s2/$s1, $s2/$s1/1.60934;
#  print $s1," ",$s2," ",$s2/$s1," ",$s2/$s1/1.60934,"\n";
}
print "\n";
$av1 /= $count;
$av2 /= $count;
$avratio /= $count;
printf "%5.0f %5.0f %5.3f %5.3f\n",$av1, $av2, $avratio*1.60934, $avratio; 

sub tosecs {
  local $tmp1;
  @frags = split(":",$_[0]);
  $tmp1 = $frags[0]*60 + $frags[1];
  $tmp1;
}
  
