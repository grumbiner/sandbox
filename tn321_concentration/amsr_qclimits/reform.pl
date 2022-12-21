#!/usr/bin/perl
#Robert Grumbine
#18 August 2010

$i=0;
while (( $line = <STDIN>) ) {
  #print $i, " ", $line;
  chop($line);

  if (($i % 6) != 0) {
    @locs = split(/\s+/,$line);
    $tlat = $locs[8];
    $tlon = $locs[9];
    #for ($j = 0; $j < $#locs; $j++) {
    #  print " ij = ",$i," ",$j," ",$locs[$j]," ";
    #}
    #print "\n";
    #print $tlat, " ", $tlon,"\n";
    $r = ($i % 6);
    if ($r == 1) {
      $lat1 = $tlat;
      $lon1 = $tlon;
    }
    elsif ($r == 4) {
      $lat2 = $tlat;
      $lon2 = $tlon;
    }
    elsif ($r == 5) {
      $lat3 = $tlat; 
      $lon3 = $tlon;
      printf("%f %f  %f %f  %f %f\n",$lat1, $lon1, $lat2, $lon2, $lat3, $lon3);
    }
  }
  #print "i = ",$i,"\n";


  $i++;
} 
