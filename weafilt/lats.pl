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
for ($i = -90; $i < 350; $i++) {
  $icecounts[$i+90] = 0;
  $landcounts[$i+90] = 0;
  $coastcounts[$i+90] = 0;
  $watercounts[$i+90] = 0;
}

$tindex = $ilat;

while ($line = <STDIN>) {
  chop($line);
  @words = split(/\s+/,$line);
  $n = $#words;

  if ($words[$n - 9] == $fland) {
    $cland += 1;
    $landcounts[ int (0.5 + $words[$n - $tindex]) + 90 ] += 1;
  }
  elsif ($words[$n - 9] == $fcoast) {
    $ccoast += 1;
    $coastcounts[ int (0.5 + $words[$n - $tindex])+ 90  ] += 1;
  }
  elsif ($words[$n - 9] == 0 && $words[$n - 10] != 0) {
    $cice += 1;
    $icecounts[ int (0.5 + $words[$n - $tindex])+ 90  ] += 1;
  }
  else {
    $cwater += 1;
    $watercounts[ int (0.5 + $words[$n - $tindex])+ 90  ] += 1;
  }

  
}

for ($i = 0; $i < 430; $i++) {
  $tot = ($watercounts[$i]+$landcounts[$i]+$coastcounts[$i]+$icecounts[$i]);
  
  if ($tot != 0) {
    printf("%3d %7d %7d %7d %7d  %5.3f %5.3f %5.3f %5.3f\n",$i - 90 , 
            $watercounts[$i], $icecounts[$i], $landcounts[$i], $coastcounts[$i],
     $watercounts[$i]/$tot,  $icecounts[$i]/$tot,  $landcounts[$i]/$tot,  $coastcounts[$i] / $tot);
  }
}

