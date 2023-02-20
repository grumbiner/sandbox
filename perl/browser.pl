#!/usr/bin/perl

while ( $line = <STDIN> ) {
  chop($line);
  $all{$line} += 1;
  @words = split(/\"/,$line);
  $n{$words[$#words]} += 1;
}

while ( ($key,$value) = each(%n) ) {
  printf ("%7d  %s\n",$value,$key);
}
exit;
print "zzzzzzzzzzzzzzzz\n";
while ( ($key,$value) = each(%windows) ) {
  print $value," Windows ",$key," \n";
}
print "zzzzzzzzzzzzzzzz\n";
while ( ($key,$value) = each(%wget) ) {
  print $value," Wget ",$key," \n";
}
print "zzzzzzzzzzzzzzzz\n";
while ( ($key,$value) = each(%mac) ) {
  print $value," Mac ",$key," \n";
}
print "zzzzzzzzzzzzzzzz\n";
while ( ($key,$value) = each(%linux) ) {
  print $value," Linux ",$key," \n";
}
exit;
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
