#!/usr/bin/perl
#
$fland = 157;
$fcoast = 195;
$fwater = 0;
$fice   = 1;

$iconc = 10;
$itype = 9;
$ilat  = 8;
$ilon  = 7;
$it19v = 6;
$it19h = 5;
$it22v = 4;
$it37v = 3;
$it37h = 2;
$it85v = 1;
$it85h = 0;


$cland  = 0;
$ccoast = 0;
$cwater = 0;
$cice   = 0;
for ($i = 0; $i < 350; $i++) {
  $icecounts[$i] = 0;
  $landcounts[$i] = 0;
  $coastcounts[$i] = 0;
  $watercounts[$i] = 0;
}

$tindex = 220;
$count = 0;

while ($line = <STDIN>) {
  chop($line);
  @words = split(/\s+/,$line);
  $n = $#words;
  $val=(1==1);
  for ($i = 0; $i < 7; $i++) {
    $val = $val && ($words[$n - $i] < $tindex);
  }
  if ($val) {
    print $line,"\n";
    $count += 1;
  }

}
print $count,"\n";
