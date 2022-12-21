#!/usr/bin/perl -w
#!/usr/bin/perl -wT

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

my $filename = $query->param("source");

my $title = $query->param("title");
my $additional = $query->param("additional");
my $authors = $query->param("authors");
my $affiliation = $query->param("affiliation");
my $email = $query->param("email");
my $year = $query->param("year");
my $keywords = $query->param("keywords");
my $status = $query->param("status");
my $abstract = $query->param("abstract");

if ( !$filename ) {
 print $query->header ( );
 print "There was a problem uploading your paper (try a smaller file).";
 exit;
}

my ( $name, $path, $extension ) = fileparse ( $filename, '\..*' );
$filename = $name . $extension;
$filename =~ tr/ /_/;
$filename =~ s/[^$safe_filename_characters]//g;

if ( $filename =~ /^([$safe_filename_characters]+)$/ )
{
 $filename = $1;
}
else
{
 die "Filename contains invalid characters";
}

my $upload_filehandle = $query->upload("source");

open ( UPLOADFILE, ">$upload_dir/$filename" ) or die "$!";
binmode UPLOADFILE;

while ( <$upload_filehandle> )
{
 print UPLOADFILE;
}

close UPLOADFILE;

print $query->header ( );
print <<END_HTML;
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
 <head>
   <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
   <title>Thanks!</title>
   <style type="text/css">
     img {border: none;}
   </style>
 </head>
 <body>
   <p>Thanks for uploading 
     <a href="/tmp/$filename" alt="file" />your paper!</a><br>
 </body>
</html>
END_HTML

# Now make the xml file for use elsewhere:

open(addcount , '<', "/data/WWW/cgi-bin/addcount") or die 
                  "Can't open addcount file";
open(addcount2 , '>', "/data/WWW/cgi-bin/addcount2") or die 
                  "Can't open addcount2 file";

#addcount is the file with the pre-existing count
my $tmp = <addcount>;
$tmp += 1;
print addcount2 $tmp;
system "/bin/cp addcount2 addcount";
print "your document number ",$tmp,"\n";

my $tname=$tmp.".xml";
if ( $tname =~ /^([$safe_filename_characters]+)$/ )
{
 $tname = $1;
}
else
{
  die "Unsafe file name ",$tname,"\n";
}
open alpha, ">$upload_dir/$tname";
print alpha <<END_XML; 
<?xml version='1.0' encoding='iso-8859-1'?>
<?xml-stylesheet href="/mmab/images/paper.xsl" type="text/xsl" ?>

<document>
<number>$tmp</number>
<authors>
<author>$authors</author>
<affiliation>$affiliation</affiliation>
<email>$email</email>
</authors>
<title>$title</title>
<additional>$additional</additional>
<mmab_note>$tmp</mmab_note>
<year>$year</year>
<keywords>$keywords</keywords>
<status>$status</status>
<abstract>$abstract</abstract>
<link>/mmab/additional_contributions/an$tmp/$filename</link>
</document>

END_XML

# Now that file has been uploaded and xml created, move to papers directory
system "mkdir /data/WWW/mmab/additional_contributions/an$tmp";
system "/bin/cp $upload_dir/$tname $upload_dir/$filename /data/WWW/mmab/additional_contributions/an$tmp";
#print "/bin/cp $upload_dir/$tname $upload_dir/$filename /data/WWW/mmab/additional_contributions/an$tmp";

#Finally, run update of summary index:
open beta, ">>$upload_dir/added.notes";
print beta <<END2_XML; 
<document>
<number>$tmp</number>
<authors>
  <author>$authors</author>
  <affiliation>$affiliation</affiliation>
  <email>$email</email>
</authors>
<title>$title</title>
<additional>$additional</additional>
<mmab_note>$tmp</mmab_note>
<year>$year</year>
<keywords>$keywords</keywords>
<status>$status</status>
<abstract>$abstract</abstract>
<link>/mmab/additional_contributions/an$tmp/$tmp.xml</link>
</document>

END2_XML

open addedheader, ">$upload_dir/addedheader";
print addedheader "<?xml version='1.0' encoding='iso-8859-1'?>\n";
print addedheader "<?xml-stylesheet href=\"/mmab/images/paper.xsl\" type=\"text/xsl\" ?>\n";
print addedheader "<biglist>\n";

open beta, "<$upload_dir/added.notes";
while (my $line = <beta>) {
  print addedheader $line;
}

print addedheader "</biglist>\n";

#Now ready to translate header in to a new index.html
system "$upload_dir/addupdate.sh $tmp";
