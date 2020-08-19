#!/usr/bin/perl

#Read through the physical-navy matchups and average
#  the differences in bins by navy temperature 
#Alt: do the binning by distance

$navy = 5;
$physical = 6;
$blend    = 10;
$distance = 9;

$index = $distance;
if ($index == $distance ) { 
  $scale = .1;
  $rebin = 10;
}
else {
  $scale = 100;
  $rebin = 1;
}

while ($line = <STDIN>) {
  chop($line);
  @words = split(/\s+/,$line);
  if ($index == $distance ) { 
    $ref = int (0.5 + $words[$index]/$rebin);
  }
  else {
    $ref = $words[$index]/$rebin;
  }
  $count{$ref} += 1;
  $deltas{$ref} += $words[11];
  $d2{$ref} += $words[11]*$words[11];
}
while ( ($key, $value) = each(%count) )  {
  #key = temperature, $value = count
  $mean = $deltas{$key}/$value;
  $stdev = sqrt(($d2{$key} - $value * $mean*$mean)/$value);
  if ($stdev > 0) {
    printf("%7.2f %6d  %7.2f %7.3f %f\n",$key/$scale, $value, $mean/100., $stdev/100., $mean / $stdev / sqrt(2/$value) );
    #see charney and devore, p 289, n = m
  }
  else {

    printf("%7.2f %6d  %7.2f %7.3f %f\n",$key/$scale, $value, $mean/100., $stdev/100., 0.0);
  }
}
#
# match  238 12269 506 3250 2911 dist (km)  183.4099274 2945 -339
# match  166 12365 506 3270 2929 dist (km)  104.8191681 3125 -341
# match  -2618 11173 645 2410 2059 dist (km)  136.8587646 2193 -351
# match  -2576 11195 645 2450 2095 dist (km)  100.6496277 2308 -355
