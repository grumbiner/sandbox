#!/usr/bin/perl

$critical = $ARGV[0];

print "\<?xml version=\"1.0\" encoding=\"UTF-8\"?\>\n";
print "\<kml xmlns=\"http://earth.google.com/kml/2.2\"\>\n";
print "\<Folder\>\n";
print "  \<name\>30 minute grid $critical delta T points\</name\>\n";
print "  \<LookAt\>\n";
print "    <longitude>100</longitude>\n";
print "    <latitude>35</latitude>\n";
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

  if ($words[10] > $critical || $words[10] < -1.*$critical) {
  $t1 = $words[8]-273.15;
  $t2 = $words[9]-273.15;
  print " \<Placemark\>\n";
  print "\<name\> ";
  if ($line =~ /lo/) {
    printf "l %3d %3d %3d ",$words[5],$words[6],$words[7];
  }
  elsif ($line =~ /hi/) {
    printf "h %3d %3d %3d ",$words[5],$words[6],$words[7];
  }
  elsif ($line =~ /rtg/) {
    printf "r %3d %3d %3d ",$words[5],$words[6],$words[7];
  }
  else {
    print "Unknown code, exiting\n";
    print $line;
    exit;
  } 
  printf "%5.2f %5.2f %6.2f %1d",$t1,$t2, $words[10],$words[11];
  print " \</name\>\n";
  print "  \<Point\>\n";
  $lat = $words[3];
  $lon = $words[4];
  if ( $lon > 180 ) {
    $lon = $lon - 360;
    #$lon = $lon . "W"; 
  }
  print "  \<coordinates\>",$lon,",",$lat,", 0\</coordinates\>\n";
  print "  \</Point\>\n";
  print "\</Placemark\>\n";
  }
  else {
    #print $words[10]," versus ",$critical,"\n";
  }

}
print "  \</Folder\>\n";
print "\</kml\>\n";
