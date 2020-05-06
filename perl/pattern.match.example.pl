#!/usr/bin/perl

#Example of pattern matching to strings.  Note that the \\ is required
#  to escape the \, which is needed to escape s to \s to match 'spaces'.
$x="\\s+a";
$y="  a";
if ($y =~ $x ) {
  print "pattern match\n";
}
else {
  print "failed";
  print "x = ",$x;
}
  print "\n";

if ($y =~ /\s+a/ ) {
  print "pattern match\n";
}
else {
  print "failed\n";
}
