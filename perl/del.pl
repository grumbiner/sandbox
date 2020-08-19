#!/usr/bin/perl

while ($line = <STDIN> ) {
  @words = split(/\s+/,$line);
  $lat=$words[1];
  $lon=$words[2];
  $tlow = $words[8] - 273.15;
  $tnav = $words[9] - 273.15;
  $thi  = $words[10] - 273.15;
  $drtg = $thi - $tlow;
  printf("%6.2f %6.2f  %6.2f %6.2f %6.2f  %6.2f ",$lat, $lon, $tlow, $tnav, $thi, $drtg);

  if ($tnav > -5) {
    $dhin = $thi - $tnav;
    $dlon = $tlow - $tnav;
    printf("%6.2f %6.2f\n",$dhin, $dlon);
  }
  else {
    $dhin = "na";
    $dlon = "na";
    printf("na     na \n");
  }
}

