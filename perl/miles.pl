#!/usr/bin/perl

#Let argv[0] = percentage increase per week
#    argv[1] = number of weeks before dropping back to miles of 2 weeks earlier 
#    argv[2] = initial week's base value

$pct    = $ARGV[0];
$length = $ARGV[1];
$base   = $ARGV[2];

#print $pct," ",$length,"\n";

$miles[0] = $base;

$i = 0;
printf "%2d %5.1f\n",$i+1,$miles[$i];

for ($i = 1; $i < 52; $i++) {
  if ( ($i % $length) == 0) {
    $miles[$i] = $miles[$i-2];
    print "\n";
  }
  else {
    if (($i % $length) == 1 && $i > $length ) {
      $miles[$i] = (1.+$pct/100.)*$miles[$i-2];
    }
    else {
      $miles[$i] = (1.+$pct/100.)*$miles[$i - 1];
    }
  }
  printf "%2d %5.1f\n",$i+1,$miles[$i];
}

