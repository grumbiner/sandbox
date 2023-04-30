#!/usr/bin/perl


print "<?xml version=\"1.0\" ?>\n";
print "<?xml-stylesheet href=\"b.xsl\" type=\"text\/xsl\" ?>\n";

print "\<directory>\n";

for ($i = 0; $i <= $#ARGV; $i++) {

  print "<link type=\"plan\">\n";
  print "<text>Plan view graphics</text>\n";
  print "<ref>$ARGV[$i]/plan.index</ref>\n";
  print "</link>\n";

  print "<link type=\"xsec\">\n";
  print "<text>Cross Sections</text>\n";
  print "<ref>$ARGV[$i]/xsec.index</ref>\n";
  print "</link>\n";
}

print "</directory>\n";
