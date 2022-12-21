#!/usr/bin/perl


print "<xsl:stylesheet version=\"1.0\" xmlns:xsl=\"http:\/\/www.w3.org\/1999\/XSL\/Transform\"> \n";
print "<xsl:output method=\"xml\"\/>\n";
print "<xsl:template match=\"directories\">\n";

for ($i = 0; $i <= $#ARGV; $i++) {
  print "  <xsl:copy-of select=\"document('$ARGV[$i]\/indexall.xml')\"\/>\n";
}

print "<\/xsl:template>\n";
print "<\/xsl:stylesheet>\n";
