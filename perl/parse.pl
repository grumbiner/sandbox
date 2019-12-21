#!/usr/bin/perl

use XML::LibXML;
my $parser = new XML::LibXML;
$parser->validation(1);
my $doc = $parser->parse_file(shift @ARGV);

if ( $@ ) {
  print "parse errors\n",$@;
}
else {
  print "valid file\n";
}
