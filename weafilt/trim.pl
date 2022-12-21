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

$tmin[0] = 139;
$tmin[1] = 144;
$tmin[2] = 125;
$tmin[3] = 168;
$tmin[4] = 176;
$tmin[5] =  96;
$tmin[6] = 173;

$tmax[0] = 274;
$tmax[1] = 280;
$tmax[2] = 265;
$tmax[3] = 268;
$tmax[4] = 272;
$tmax[5] = 265;
$tmax[6] = 272;


while ($line = <STDIN>) {
  chop($line);
  @words = split(/\s+/,$line);
  $n = $#words;
  # polarization check:
  #if (($words[$n - $it85h] < $words[$n - $it85v]) &&
  #    ($words[$n - $it37h] < $words[$n - $it37v]) &&
  #    ($words[$n - $it19h] < $words[$n - $it19v]) 
  #)  {

  #$x = (1 == 1);
  #for ($i = 0; $i < 7; $i++) {
  #  $x = $x && ($words[$n - $i] > $tmax[$i] || $words[$n - $i] < $tmin[$i] );
  #}
  #if ($x) { 
  #  print $line,"\n";
  #}

  if ($words[$n - $ilat] < -30.0 || $words[$n - $ilat] > 20.0) {
    print $line, "\n";
  } 

}
