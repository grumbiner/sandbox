#!/usr/bin/perl

while ($line = <STDIN> ) {
  if ($line =~ /^mail\.(\S+)/ ) {
    #print "would rename ", $line," \n";
    #print "rest of line is ",$1,"\n";
    system "/usr/local/bin/ifm mail.$1 webmail.$1"
  }
  else {
    print "not renaming ", $line," \n";
  }
}
