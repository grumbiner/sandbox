#!/usr/bin/perl

$critical = $ARGV[0];

print "\<?xml version=\"1.0\" encoding=\"UTF-8\"?\>\n";
print "\<kml xmlns=\"http://earth.google.com/kml/2.2\"\>\n";
print "\<Folder\>\n";
print "  \<name\>delta T over $critical points\</name\>\n";
print "  \<LookAt\>\n";
print "    <longitude>-85</longitude>\n";
print "    <latitude>45</latitude>\n";
print "    \<range\>6400000\</range\>\n";
print "  \</LookAt\>\n";


while ( $line = <STDIN> ) {
  @words=split(/\s+/,$line);
  $n = $#words;
#      1      2    3   4    5       6       7    8       9
#     lat    lon   nl icec tnav   rtg30   delta  crit  days
#low  40.75  40.25  3 icec 288.30 283.35   -4.95 0      59

  if ($line =~ /low/  && $words[3] == 0 ) {
    if ( (abs($words[7]) > $critical) || 
         (($words[4] > 0 && $words[4] < 101) && $words[6] > 275.15) ) {
      $t1 = $words[5] - 273.15;
      $t2 = $words[6] - 273.15;
      print " \<Placemark\>\n";
      print "\<name\> ";
        printf "n %1d %3d %1d %3d \n",$words[3], $words[4], $words[8], $words[9];
      printf "%5.2f %5.2f  %6.2f\n",$t1,$t2, $words[7];
      print " \</name\>\n";
      print "  \<Point\>\n";
      $lat = $words[1];
      $lon = $words[2];
      if ( $lon > 180 ) {
        $lon = $lon - 360;
        #$lon = $lon . "W"; 
      }
      print "  \<coordinates\>",$lon,",",$lat,", 0\</coordinates\>\n";
      print "  \</Point\>\n";
      print "\</Placemark\>\n";
    }
    else {
      #print $words[9]," versus ",$critical,"\n";
    }

  }


}
print "  \</Folder\>\n";
print "\</kml\>\n";
