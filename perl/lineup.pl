#!/usr/bin/perl

$rate = 0.0625;

while ($line = <STDIN>) {
  chop($line);
  @words = split(/\s+/, $line);
  $n = $#words;
  #print $n," ", $words[$n]," ", $words[$n -1]," ", $words[$n - 2],"\n";
  #print &tosecs($words[$n-1])," ",&tosecs($words[$n - 2]),"\n";
  $delta = &tosecs($words[$n-1]) - &tosecs($words[$n - 2]);
  $errdel = $delta - &max(0., $rate*(&tosecs($words[$n-1]) - 2250.) );
  print $delta, " ",&tosecs($words[$n-1]),' ', $errdel, "\n"; 
  if ($errdel < -30) {
    print $line,"\n";
  }
}

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
