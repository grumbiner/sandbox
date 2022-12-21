#!/usr/bin/perl

print "\<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
print "\<kml xmlns=\"http://earth.google.com/kml/2.2\"\>\n";
print "\<Folder\>\n";
print "<name> Inland water of the world</name>\n";
print "<LookAt>\n";
print "<range> 12500000</range>\n";
print "<latitude>55.0</latitude>\n";
print "<longitude>-95.0</longitude>\n";
print "</LookAt>\n";

print "<Style id=\"big\"> <IconStyle> <Icon>
    <href>http://maps.google.com/mapfiles/kml/paddle/pink-blank.png</href>
  </Icon>
    <scale>3.0</scale> </IconStyle> </Style>\n";

print "<Style id=\"pink\"> <IconStyle> <Icon>
    <href>http://maps.google.com/mapfiles/kml/paddle/pink-blank.png</href>
  </Icon>
    <scale>1.0</scale> </IconStyle> </Style>\n";

print "<Style id=\"yellow\"> <IconStyle> <Icon>
    <href>http://maps.google.com/mapfiles/kml/paddle/ylw-blank.png</href>
  </Icon>
    <scale>1.0</scale> </IconStyle> </Style>\n";

print "<Style id=\"blue\"> <IconStyle> <Icon>
    <href>http://maps.google.com/mapfiles/kml/paddle/blu-blank.png</href>
  </Icon>
    <scale>1.0</scale> </IconStyle> </Style>\n";

print "<Style id=\"smblue\"> <IconStyle> <Icon>
    <href>http://maps.google.com/mapfiles/kml/paddle/blu-blank.png</href>
  </Icon>
    <scale>0.5</scale> </IconStyle> </Style>\n";



$lat = 1;
$lon = 2;
$rtg = 5;
$estim = 6;
$delta = 7;

while ($line = <STDIN> ) {
  #@words=split(/\s+/,$line);
  chop($line);
  @words=split(/\s+/,$line);
  print "<Placemark>\n";
  
  if (abs($words[$delta]) < 5 && $words[$rtg] < 31) { 
    if (abs($words[$delta]) < 2) {
      print "<styleUrl>#smblue</styleUrl>\n";
    }
    else {
      print "<styleUrl>#blue</styleUrl>\n";
    }
  }
  elsif (abs($words[$delta]) >= 9 || $words[$rtg] >= 34.0) {
    if (abs($words[$delta]) >= 14 || $words[$rtg] >= 35.0) {
      print "<styleUrl>#big</styleUrl>\n";
    }
    else {
      print "<styleUrl>#pink</styleUrl>\n";
    }
  }
  else {
    print "<styleUrl>#yellow</styleUrl>\n";
  }


  print "<description>\n";
  for ($i = 8; $i<=$#words; $i++) {
    print $words[$i]," ";
  }
  print "\n";
  print "rtg = ",$words[$rtg],"\nestim = ",$words[$estim],"\ndelta = ",$words[$delta],"\n";
  print "</description>\n";
  print "<Point>\n";
  if ($words[$lon] > 180) {
    $words[$lon] -= 360.;
  }
  print "<coordinates> ",$words[$lon],", ",$words[$lat],"</coordinates>\n";
  print "</Point>\n";
  print "</Placemark>\n";

}

print "\</Folder\>\n";
print "\</kml\>\n";
