#!/usr/bin/perl

$sum = 0;
$sumsq = 0;
$count = 0;

while ($line = <STDIN>) {
  chop($line);
  @words = split(/\s+/, $line);
  $count += 1;

  $rat = &tosecs($words[1]) / &tosecs($words[0]);
  $sum += $rat;
  $sumsq += $rat*$rat;

  print &tosecs($words[0])," ", 
        &tosecs($words[1])," ", 
        $rat, "\n";
}
print $sum/$count, " ", sqrt($sumsq/$count), "\n";

sub tosecs {
  local $tmp1;
  @times=split(/:/, $_[0]);
  $tmp1=$times[0]*60 + $times[1];
  #print $#times," ",$tmp1," secs ",$times[0]," ", $times[1],"\n";
  $tmp1;
} 

sub max {
  if ( $_[1] > $_[0] ) {
    $_[1];
  }
  else {
    $_[0];
  }
}
