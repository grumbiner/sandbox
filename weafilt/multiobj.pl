#!/usr/bin/perl

#Look for anything not dominated by prior solutions and print it out

$best_ice_pod   = 0.2;
$best_water_pod = 0.2;
$best_ice_far   = 1.0;
$best_water_far = 1.0;

while ($line = <STDIN>) {
  if ($line =~ /tc/) {
    chop($line);
    @stats = split(/\s+/,$line);
    $icepod = $stats[9];
    $icefar = $stats[10];
    $waterpod = $stats[12];
    $waterfar = $stats[13];
    $iceratio = $stats[15];
    $waterratio = $stats[16];

    #printf("%f %f %f %f\n",$icepod, $icefar, $waterpod, $waterfar);

    if ($icepod >= $best_ice_pod && $iceratio > 1 ) {
      print "ice ",$line,"\n";
      $best_ice_pod = $icepod;
    }
    if ($icefar <= $best_ice_far && $iceratio > 1) {
      print "ice ",$line,"\n";
      $best_ice_far = $icefar;
    }
    if ($waterpod >= $best_water_pod && $waterratio > 1.5) {
      print "water ",$line,"\n";
      $best_water_pod = $waterpod;
    }
    if ($waterfar <= $best_water_far && $waterratio > 1.5 && $waterpod > 0.02) {
      print "water ",$line,"\n";
      $best_water_far = $waterfar;
    }

  }
     
}

print "Final bests: \n";
printf("%f %f %f %f\n",$best_ice_pod, $best_ice_far, $best_water_pod, $best_water_far);
