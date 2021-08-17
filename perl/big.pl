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

 #LARGE SST =    41.530 AT POINT  3601  330 LON =  300.042 LAT =  -62.542
  $temp = $words[4];
  $lon  = $words[11];
  $lat  = $words[14];
  #print $temp, " ",$lat, " ",$lon,"\n";
  
  print " \<Placemark\>\n";
  print "\<name\> ";
  printf "%7.2f\n", $temp; 
  print " \</name\>\n";
  print "  \<Point\>\n";
  if ( $lon > 180 ) {
    $lon = $lon - 360;
    #$lon = $lon . "W"; 
  }
  print "  \<coordinates\>",$lon,",",$lat,", 0\</coordinates\>\n";
  print "  \</Point\>\n";
  print "\</Placemark\>\n";
 

}
print "  \</Folder\>\n";
print "\</kml\>\n";
