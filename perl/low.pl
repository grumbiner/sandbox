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
  #print "n = ",$n," ",$words[$n],"\n";
  #for ($i = 0; $i <= $n; $i++) {
  #  print "i = ",$i," words[i] = ",$words[$i],"\n";
  #}
  #exit;
#compout.20090106:low  34  27   76.25  17.25    1    0   0   40  275.70 271.42   -4.280
#      1      2       3  4    5    6     7      8        9
#      lat    lon     n  il   rl  ice   navy  rtg30     delta
#low  30.75  90.75    3    0 157    0  275.03 289.52   14.487


  #print "words 9, critical ",$words[9]," ",$critical,"\n";

  if ($line =~ /low/) {

  if ($words[9] > $critical || $words[9] < -1.*$critical) {
  $t1 = $words[7]-273.15;
  $t2 = $words[8] - 273.15;
  print " \<Placemark\>\n";
  print "\<name\> ";
    printf "n %3d %3d %1d  %3d\n",$words[3], $words[4], $words[5],$words[6];
  printf "%5.2f %5.2f  %6.2f\n",$t1,$t2, $words[9];
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
