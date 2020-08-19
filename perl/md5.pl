#!/usr/bin/perl
#Robert Grumbine 8 June 2004

use Digest::MD5;

open(FILE, $ARGV[1]);
binmode(FILE);

$md5 = Digest::MD5->new;
while (<FILE>) {
  $md5->add($_);
}
close(FILE);
print $md5->b64digest,"\n";


$md5 = Digest::MD5->new;
open(FILE, $ARGV[1]);
binmode(FILE);
print Digest::MD5->new->addfile(*FILE)->b64digest,"\n";
close(FILE);

