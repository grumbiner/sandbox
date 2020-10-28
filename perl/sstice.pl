#!/usr/bin/perl -w
#!/usr/bin/perl -wT

# For user/observers to upload comments about sea ice and sst observations
# Robert Grumbine 30 August 2012
#derives from repository.pl

use strict;
use CGI;
use CGI::Carp qw ( fatalsToBrowser );
use File::Basename;

#This is a byte count limit on file size:
$CGI::POST_MAX = 1024 * 1024 * 15;
my $safe_filename_characters = "a-zA-Z0-9_.-";
#Note that at this point, we're using absolute path.   RG
my $upload_dir = "/data/WWW/tmp/";

my $query = new CGI;
my $username = $query->param("username");
my $addr1 = $query->param("addr1");
my $addr2 = $query->param("addr2");
my $date  = $query->param("date");
my $location = $query->param("location");
my $name = $query->param("name");
my $comment = $query->param("comment");


print $query->header ( );
print <<END_HTML;
<html>
 <head>
   <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
   <title>Thanks!</title>
   <style type="text/css"> img {border: none;} </style>
 </head>
 <body>
   <p>Thanks for uploading your observation!
 </body>
</html>
END_HTML

# Now make the xml file for use elsewhere:

open(obscount , '<', "/data/WWW/cgi-bin/obscount") or die 
                  "Can't open obscount file";
open(obscount2 , '>', "/data/WWW/cgi-bin/obscount2") or die 
                  "Can't open obscount file";

#obscount is the file with the pre-existing count
my $tmp = <obscount>;
$tmp += 1;
print obscount2 $tmp,"\n";
system "/bin/cp obscount2 obscount";
print "your observation number ",$tmp,"\n";

my $tname=$tmp.".xml";
if ( $tname =~ /^([$safe_filename_characters]+)$/ ) {
 $tname = $1;
}
else {
  die "Unsafe file name ",$tname,"\n";
}
open alpha, ">$upload_dir/$tname";
print alpha <<END_XML; 
<?xml version='1.0' encoding='iso-8859-1'?>
<?xml-stylesheet href="/mmab/images/obscomment.xsl" type="text/xsl" ?>

<obscomment>
<number>$tmp</number>
<Observer>
<username>$username</username>
<addr1>$addr1</addr1>
<addr2>$addr2</addr2>
</Observer>
<Location>
<ll>$location</ll>
<name>$name</name>
<Location>
<comment>$comment</comment>
</document>

END_XML


#Now notify that an observation is available:
#system "mail -s \"Observation $tmp available\" robert.grumbine\@noaa.gov \< /data/WWW/cgi-bin/obscount";
