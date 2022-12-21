#!/usr/bin/perl

#Try to build a concordance, while being sloppy about definition of "word"

#set up a 'stop list' -- words not to include in the concordance, this should
#  be moved to an include
@toadd=("I","a","of","to","and","com","running","net","the","X","in","is","it",
  "was","about","one","if","all","so","like","what", "out", "there");
@toadd= (@toadd,"news","you","for","radix","From","that","rec");
@toadd=(@toadd,"news1","s","t","Subject", "Posting","Message","ID","Path","as");
@toadd=(@toadd,"your","at","be","ca","my","on","or","with","Mar","NNTP","not");
@toadd=(@toadd,"Host","Apr","nx1","this","Article","are","Newsgroups","netcom");
@toadd=(@toadd,"Organization","but","EDT","EST","can","The","have","Xref","run");
@toadd=(@toadd,"Date","GMT","Fri","edu","Lines","tor","m","http","dejanews");
@toadd=(@toadd,"M","gov","F3","noaa","wwb","Jan","Feb","NW","by","id");
@toadd=(@toadd,"Received","from","F","MV","Return","rmg3","Content","ERY");
@toadd=(@toadd,"Mon","Tue","Wed","Thu","Fri","Sat","Sun");
@toadd=(@toadd,"has","SMTP","NOAA","mil","Status","nasa");
@toadd=(@toadd,"deja","Jul","nnrp1","syr","maxwell","macelroy");
@toadd=(@toadd,"References","spur1","http","Http","would","just");

foreach $j (@toadd) {
  $stopl{$j} += 1;
}

$tcount=0;
$strln = 7;
while ($line = <STDIN>) {
  #print $line;
  chop($line);
  $tcount += 1;
  if ( $line =~ /(\S{$strln})(\S{$strln})(\S{$strln})(\S{$strln})(\S{$strln})(\S{$strln})(\S{$strln})(\S{$strln})(\S{$strln})(\S{$strln})/ ) {
    $words[0] = $1;
    $words[1] = $2;
    $words[2] = $3;
    $words[3] = $4;
    $words[4] = $5;
    $words[5] = $6;
    $words[6] = $7;
    $words[7] = $8;
    $words[8] = $9;
    #for ($j = 0; $j < 9; $j++) {
    #  print $words[$j],"\n";
    #}
    foreach $i (@words) {
       if ( ! &in($i) ) {
         $wordcount{$i} += 1;
       }
    }
  }

}
print "\n";

$top = &findmax(values(%wordcount));
$bottom = 0;
print "Most commonly used word count, ",$top,"\n";

print "Pseudo-sorted most commonly used word count, ",$top,"\n";
$bottom = $top / 2;
while ( $top >= 1 ) {
#  print $top, " ",$bottom,"\n";
  print "\n";
  while ( ($key,$value) = each(%wordcount) ) {
    if ($value > $bottom && $value <= $top) {
      printf "%5d retrievals of %s\n",$value, $key;
    }
  }
  $top = $bottom;
  $bottom = $top / 2;
}

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
  #print $stopl{"that"}," stop list of that, should be 1 ";
  #print $tword," in, tempor = ",$tempor," \n";
  $tempor;
}
