#!/usr/bin/perl

#Mac interface to find files:
use Mac::Files;
use Mac::StandardFile;

#Try to build a concordance, while being sloppy about definition of "word"

#set up a 'stop list' -- words not to include in the concordance, this should
#  be moved to an include
@toadd=("I","a","of","to","and","com","running","net","the","X","in","is","it");
@toadd= (@toadd,"news","you","for","radix","From","that","rec");
@toadd=(@toadd,"news1","s","t","Subject", "Posting","Message","ID","Path","as");
@toadd=(@toadd,"your","at","be","ca","my","on","or","with","Mar","NNTP","not");
@toadd=(@toadd,"Host","Apr","nx1","this","Article","are","Newsgroups","netcom");
@toadd=(@toadd,"Organization","but","EST","can","The","have","Xref","run");
@toadd=(@toadd,"Date","GMT","Fri","edu","Lines","tor","m","http","dejanews");
@toadd=(@toadd,"M","gov","F3","noaa","wwb","Jan","Feb","NW","by","id");
@toadd=(@toadd,"Received","from","F","MV","Return","rmg3","Content","ERY");
@toadd=(@toadd,"Mon","Tue","Wed","Thu","Fri","Sat","Sun");
@toadd=(@toadd,"has","SMTP","NOAA","mil","Status","nasa");
foreach $j (@toadd) {
  $stopl{$j} += 1;
}

#Open an input file:
$info = StandardGetFile(0,"TEXT");
open(FIN, $info->sfFile);
open(FOUT, ">outfile") || die "failed to open output";

$tcount=0;
while ($line = <FIN>) {
  chop($line);
  $tcount += 1;
  @words = split(/\W+/, $line);
  foreach $i (@words) {
    if ($i =~ /[a-z]/i) {
       #Don't include strings that have no alphabetic characters
       if ( ! &in($i) ) {
         $wordcount{$i} += 1;
       }
    }
  }
}
print FOUT "\n";

$top = &findmax(values(%wordcount));
$bottom = 0;
print FOUT "Most commonly used word count, ",$top,"\n";
print  "Most commonly used word count, ",$top,"\n";

$bottom = $top / 2;
while ( $top > 3 ) {
  print  FOUT "\n";
  while ( ($key,$value) = each(%wordcount) ) {
    if ($value > $bottom && $value <= $top) {
      printf  FOUT "%5d retrievals of %s\n",$value, $key;
    }
  }
  $top = $bottom;
  $bottom = $top / 2;
}
print "Done\n";


#Utilities:
sub findmax {
  local $tmp1;
  $tmp1 = 0;
  foreach $i (@_) {
    if ($i > $tmp1) {
      $tmp1 = $i;
    }
  }
  $tmp1;
}


sub in {
  local $tempor, $tword;
  $tword = $_[0];
  $tempor = $stopl{$tword};
  $tempor;
}
