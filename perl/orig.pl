#!/usr/bin/perl

while ( $line = <STDIN> ) {
  chop($line);
  $all{$line} += 1;
#OS:
  if ($line =~ /Windows ([a-zA-Z0-9_\.\ ]+)(;|\))/) {
    $windows{$1} += 1;
  }
  if ($line =~ /\(Macintosh; ([a-zA-Z0-9_;:\.\ ]+)(\))/) {
    $mac{$1} += 1;
  }
  if ($line =~ /Mac_PowerPC/) {
    $mac{"Mac_PowerPC"} += 1;
  }
  if ($line =~ /Linux/) {
    $linux{$line} += 1;
  }
#Browser:
  if ($line =~ /MSIE (\d\.\d+)\;/) {
    $msie{$1} += 1;
  }
  if ($line =~ /Safari/) {
    $safari{$line} += 1;
  }
  if ($line =~ /Opera/) {
    $opera{$line} += 1;
  }

}
#3963  "Mozilla/4.0 (compatible; MSIE 6.0; Windows 98; Win 9x 4.90)"
#3259  "Mozilla/4.0 (compatible; MSIE 5.01; Windows NT 5.0)"
#2954  "Mozilla/4.0 (compatible; MSIE 5.5; Windows 98; Win 9x 4.90)"
#2740  "Mozilla/4.0 (compatible; MSIE 5.5; Windows 98)"
#2708  "Mozilla/4.0 (compatible; MSIE 5.5; Windows NT 5.0)"
#2027  "Mozilla/4.0 (compatible; MSIE 6.0; Windows 98; .NET CLR 1.1.4322)"
#1831  "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; FunWebProducts)"
#1613  "Mozilla/4.0 (compatible; MSIE 5.22; Mac_PowerPC)"
#1594  "Mozilla/4.0 (compatible; MSIE 5.23; Mac_PowerPC)"

#while ( ($key,$value) = each(%all) ) {
#  print $value," ",$key," \n";
#}
#exit;
print "zzzzzzzzzzzzzzzz\n";
while ( ($key,$value) = each(%windows) ) {
  print $value," Windows ",$key," \n";
}
print "zzzzzzzzzzzzzzzz\n";
while ( ($key,$value) = each(%mac) ) {
  print $value," Mac ",$key," \n";
}
print "zzzzzzzzzzzzzzzz\n";
while ( ($key,$value) = each(%linux) ) {
  print $value," Linux ",$key," \n";
}
print "zzzzzzzzzzzzzzzz\n";
print "zzzzzzzzzzzzzzzz\n";
while ( ($key,$value) = each(%msie) ) {
  print $value," MSIE ",$key," \n";
}
print "zzzzzzzzzzzzzzzz\n";
while ( ($key,$value) = each(%safari) ) {
  print $value," Safari ",$key," \n";
}
print "zzzzzzzzzzzzzzzz\n";
while ( ($key,$value) = each(%opera) ) {
  print $value," Opera ",$key," \n";
}
