#!/usr/bin/perl
#Script to process an input web page (first arg) and produce an output
#  page which follows the jump page policies of the NWS as enunciated
#  during October 2000.
#Jump page is not required if:
#  a) the link is to another NOAA site
#  b) the link is to a page on a US Governmental site which is
#        displaying data relevant to NWS mission.  ('content page')
#Jump page _is_ required if:
#  a) the link is not to a US Governmental site
#  b) the link is to a page on a US Governmental site which is _not_
#        displaying data relevant to NWS mission.  ('organization page')
#Robert Grumbine 2 November 2000

$original = $ARGV[0];
$newpage  = $ARGV[1];
open original, "<$ARGV[0]";
open newpage,  ">$ARGV[1]";

print "This script can settle most of the jump page issues automatically.\n";
print "The situation which is most difficult is one in which the link is to\n";
print "  another governmental page outside NOAA.  You'll be called on to \n";
print "  declare whether the link is for content -- data that the other site\n";
print "  has which is relevant to the NWS mission -- or 'organizational', \n";
print "  a link which is recognizing partners but not directly connecting \n";
print "  to a page with specific data.\n";
print " \n";
print "After running this script, please double check the results.  There \n";
print "  are NOAA pages which are not hosted on .gov sites (some NWS offices\n";
print "  are hosted at .edu sites, for instance) and which will be forced\n";
print "  by this script to go through jump pages even though it isn't \n";
print "  required.\n";
print " \n";
print " \n";

#print "Jump page is not required if:\n";
#print "  a) the link is to another NOAA site\n";
#print "  b) the link is to a page on a US Governmental site which is\n";
#print "        displaying data relevant to NWS mission.  ('content page')\n";
#print "Jump page _is_ required if:\n";
#print "  a) the link is not to a US Governmental site\n";
#print "  b) the link is to a page on a US Governmental site which is _not_\n";
#print "        displaying data relevant to NWS mission.  ('organization page')\n";

while ($line = <original>) {
  chop($line);

  if ( $line =~ /(.+)href(.+)http:\/\/(.+)\"(.+)/i  ){
    $first = $1;
    $site = $3;
    $third = $4;
    #Simply echo if it is a NOAA Internal site:
    #if ( $site =~ /(wwb|ncep|nws).noaa.gov/) {
    if ( $site =~ /(wwb|ncep|nws).noaa.gov/ || $line =~ /jump\.pl\?/ ) {
      print newpage $line,"\n";
    }
    else {
      if ($site =~ /\.gov/) {
        print "The site is ",$site;
        print "\n Is this a content site or an organizational site (c or o)\n";
        $ans = <STDIN>;
        if ($ans =~ /c/ && $site =~ /\.gov/) {
          print newpage $line,"\n";
        }
        else {
          print newpage $first; #re-establish the material prior to the href
          print newpage "HREF=\"http://polar.wwb.noaa.gov/cgi-bin/jump.pl?http://";
          print newpage $site,"\" ";
          print newpage $third,"\n";
        }
      }
      else { #Not a governmental site, jump required
        print newpage $first; #re-establish the material prior to the href
        print newpage "HREF=\"http://polar.wwb.noaa.gov/cgi-bin/jump.pl?http://";
        print newpage $site,"\" ";
        print newpage $third,"\n";
      }
    }
  }
  else {
    #Line doesn't have any href's, just echo it
    print newpage $line, "\n";
  }
}
