#!/usr/bin/perl

open outfile, ">hiresdif.html";
print outfile "\<!doctype html public \"-\/\/w3c\/\/dtd html 4.0 transitional\/\
/en\"\>\n";
print outfile "<HTML> <HEAD> <TITLE>\n";
print outfile "Ice drift verification for $year\n<\/TITLE>\n";
print outfile "\<BODY BGCOLOR=\"#FFFFFF\">\n";

$header[0] = "Old flag";
$header[1] = "New flag";
$header[2] = "Latitude";
$header[3] = "E Longitude";

$intable=0;
while ($line = <stdin>) {
  if ($line =~ /old new location/) {
    if ($intable != 1) {
      $intable = 1;
      &tablestart(@header);
    }
    chop($line);
    @words = split(' ', $line);
    &tabline($words[3], $words[4], $words[5], $words[6]); 
  }
  else {
   if ($intable == 1) {
     &tablefin();
     $intable = 0;
   }
   print outfile $line;
  }
}
if ($intable == 1) {
  &tablefin();
}

#############################################
sub tablestart {
  print outfile "<TABLE BORDER=2>\n";
  print outfile "<TR>\n";
  for ($i = 0; $i <= $#_; $i++) {
     print outfile "<TD>$_[$i]<\/TD>\n";
  }
  print outfile "<\/TR>\n";

}
sub tablefin {
  print outfile "</TABLE>\n";
  print outfile "<p>\n";
}
sub tabline {
  print outfile "<TR>\n";
  for ($i = 0; $i <= $#_; $i++) {
     print outfile "<TD>$_[$i]<\/TD>\n";
  }
  print outfile "<\/TR>\n";
}
