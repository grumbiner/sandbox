#!/usr/bin/perl

#Search through for would-be urls and bracket them

while ($line = <STDIN>) {
  chop($line);
  #if ($line =~ /http:\/\/(.+)/i ) {
  if ($line =~ /http:\/(.+)/i ) {
    $tmp1 = $1;
    if ($tmp1 =~ /(\/\S+)/ ) {
      print "<LI><A HREF=\"http:/",$1,"\"\>",$1,"</A>\n";
    }
    else {
       print "nope ",$tmp1,"\n";
    }
  }
}
