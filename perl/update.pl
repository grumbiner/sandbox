#!/usr/bin/perl

#read file name for the tech note number,
#  then update the corresponding xml file to include a link, copy the pdf,
#  and generally do updating stuff

$line = $ARGV[0];
#print $line,"\n";

if ($line =~ /(\D+)(\d+).pdf/ ) {
  $base = $1;
  $num  = $2;
  #print "hello \n";
  #print $base," ", $num,"\n";
  print "vi ~/www/mmab/papers/tn",$num,"/paper.xml\n";
  print "mv $line  ~/www/mmab/papers/tn",$num,"\n";
}
else {
  print "oops ",$line,"\n";
}

