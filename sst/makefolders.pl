#!/usr/bin/perl

$critical = $ARGV[0];


sub folder_head {
  $tfout = $_[0];
  $tag  = $_[1];
  $tcrit = $_[2];
  #print "in folder_head, tfout = ",$tfout,"\n";
  print $tfout "\<Folder\>\n";
  print $tfout "  \<name\>$tag |dT| over $tcrit\</name\>\n";
  print $tfout "  \<LookAt\>\n";
  print $tfout "    <longitude>-85</longitude>\n";
  print $tfout "    <latitude>45</latitude>\n";
  print $tfout "    \<range\>6400000\</range\>\n";
  print $tfout "  \</LookAt\>\n";
}

$ntags = 0;
while ( $line = <STDIN> ) {
  @words=split(/\s+/,$line);
  $n = $#words;
  #print $n,"\n";
  $tag = $words[0];
  if ($ntags == 0) {
    $ntags += 1;
    $entries{$tag} += 1;
    print "opening ",$tag,"\n";
    open $fout{$tag}, ">>$tag";
    &folder_head($fout{$tag}, $tag, $critical);
  }
  if ($entries{$tag} == 0) {
    $ntags += 1;
    print "opening2 ",$tag,"\n";
    open $fout{$tag}, ">>$tag";
    &folder_head($fout{$tag}, $tag, $critical);
  }

  $entries{$tag} += 1;

  #$tfout = $fout{$tag};
  

  if ($n == 5) { 
    #then in the newer sort of comparison
    #print "should call regout\n";
    $delta = $words[5];
    #print " delta, critical = ",$delta, " ", $critical, "\n";
    if ($delta > $critical || $delta < -1.*$critical) {
      #print "passed if, should call regout\n";
      $t1 = $words[3];
      $t2 = $words[4];
      $lat = $words[1];
      $lon = $words[2];
      if ( $lon > 180 ) {
        $lon = $lon - 360;
        #$lon = $lon . "W"; 
      }
      #print "calling regout\n";
      &regout($fout{$tag}, $t1, $t2, $lat, $lon, $delta, $tag);
    }
  }

  if ($tag =~ /low/) {
    if ($words[9] > $critical || $words[9] < -1.*$critical) {
    $t1 = $words[7] - 273.15;
    $t2 = $words[8] - 273.15;
    $lat = $words[1];
    $lon = $words[2];
    if ( $lon > 180 ) {
      $lon = $lon - 360;
      #$lon = $lon . "W"; 
    }
    print $tfout " \<Placemark\>\n";
    print $tfout "\<name\> ";
      printf $tfout "n %3d %3d %1d  %3d\n",$words[3], $words[4], $words[5],$words[6];
    printf $tfout "%5.2f %5.2f  %6.2f\n",$t1,$t2, $words[9];
    print $tfout " \</name\>\n";
    print $tfout "  \<Point\>\n";
    print $tfout "  \<coordinates\>",$lon,",",$lat,", 0\</coordinates\>\n";
    print $tfout "  \</Point\>\n";
    print $tfout "\</Placemark\>\n";
    }
  } #end of 'low'

}
#Close all folders:
while ( ($key, $value) = each(%fout) ) {
  #print "in closure loop ",$key," ",$value,"\n";
  print $value "  \</Folder\>\n";
}

#Print out header information to stdout:
print "\<?xml version=\"1.0\" encoding=\"UTF-8\"?\>\n";
print "\<kml xmlns=\"http://earth.google.com/kml/2.2\"\>\n";
print "\<Document\>\n";

#Concatenate each folder into stdout:
while ( ($key, $value) = each(%fout) ) {
  #print "in system loop ",$key," ",$value,"\n";
  system "cat $key";
}

#Now close off the document:
print "\</Document\>\n";
print "\</kml\>\n";




sub regout {
  $tfout = $_[0];
  $t1    = $_[1];
  $t2    = $_[2];
  $lat   = $_[3];
  $lon   = $_[4];
  $delta = $_[5];
  $tag   = $_[6];
  #print "in regout, tfout = ",$tfout,"\n";
  #print $t1," ",$t2," ",$lat," ",$lon," ",$delta," ",$tag,"\n";
  print $tfout " \<Placemark\>\n";
  print $tfout "\<name\> $tag ";
  printf $tfout "%5.2f %5.2f  %6.2f\n",$t1,$t2, $delta;
  print $tfout " \</name\>\n";
  print $tfout "  \<Point\>\n";
  print $tfout "  \<coordinates\>",$lon,",",$lat,", 0\</coordinates\>\n";
  print $tfout "  \</Point\>\n";
  print $tfout "\</Placemark\>\n";
}
