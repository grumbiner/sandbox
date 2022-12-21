#!/usr/bin/perl

#read in a typical score file and write out a set of web pages
#summarizing the scores.

#Score file order:
# 1 line of bulk statistics, all leads, all points, all months
# header
# N lines of bulk statistics by lead, all months, all points
# blank line
# header
# 12 lines of statistics by month, all points, all leads
# blank line
# header
# header
# pointwise statistics, by forecast lead, all months

$year = $ARGV[0];
open infile, "<$ARGV[1]";
open outfile, ">score$year.html";

#Print out header:
print outfile "\<!doctype html public \"-\/\/w3c\/\/dtd html 4.0 transitional\/\/en\"\>\n";
print outfile "<HTML> <HEAD> <TITLE>\n";
print outfile "Ice drift verification for $year\n<\/TITLE>\n";
print outfile "\<BODY BGCOLOR=\"#FFFFFF\">\n";

##############################################################
#parse the data file into table elements:
$header[0] = "Number of observations";
$header[1] = "Forecast lead";
$header[2] = "Index of agreement";
$header[3] = "Scalar correlation";
$header[4] = "Vector correlation";
$header[5] = "b0";
$header[6] = "b1";
$header[7] = "Distance Bias";
$header[8] = "Direction Bias";
$header[9] = "RMS Distance";
$header[10] = "RMS Direction";
$header[11] = "Mean Error Radius";
$header[12] = "RMS Error Radius";
#use 0, 1, 3, 4, 6, 10, 11


# first up, get the forecast lead statistics
$line = <infile>;
$line = <infile>;
$lineno = 2;
print outfile "<H3>Statistics for all months of $year by forecast lead<\/H3>\n";
#&tablestart(@header);
  print outfile "<TABLE BORDER=2>\n";
  print outfile "<TR>\n";
  print outfile "<TD>$header[0]<\/TD>\n";
  print outfile "<TD>$header[1]<\/TD>\n";
  print outfile "<TD>$header[3]<\/TD>\n";
  print outfile "<TD>$header[4]<\/TD>\n";
  print outfile "<TD>$header[6]<\/TD>\n";
  print outfile "<TD>$header[10]<\/TD>\n";
  print outfile "<TD>$header[11]<\/TD>\n";
  print outfile "<\/TR>\n";

while ($line = <infile> ) {
  chop($line);
  @words = split(" ", $line);
  #for ($i = 0; $i < $#words; $i++) {
  #   print $i," ", $#words, " ", $words[$i],"\n";
  #}
  if ($#words < 9) {last;}
  &tabline($words[0], $words[1], $words[3], $words[4], 
           $words[6], 
           $words[10], $words[11]);  
  $lineno += 1;
}
&tablefin();

#Forecasts by month:
$line=<infile>;
print outfile "<H3>Forecasts at all lead times, scored by month for year $year<\/H3>\n";
$header[1] = "Month";
#&tablestart(@header);
  print outfile "<TABLE BORDER=2>\n";
  print outfile "<TR>\n";
  print outfile "<TD>$header[0]<\/TD>\n";
  print outfile "<TD>$header[1]<\/TD>\n";
  print outfile "<TD>$header[3]<\/TD>\n";
  print outfile "<TD>$header[4]<\/TD>\n";
  print outfile "<TD>$header[6]<\/TD>\n";
  print outfile "<TD>$header[10]<\/TD>\n";
  print outfile "<TD>$header[11]<\/TD>\n";
  print outfile "<\/TR>\n";
while ($line = <infile> ) {
  chop($line);
  @words = split(" ", $line);
  if ($#words < 9 && $words[0] != -1 ) {last;}
  if ($words[0] == -1) {
    for ($i = 2; $i < 12; $i++) {
       $words[$i] = "NA";
    }
  }
  &tabline($words[0], $words[1], $words[3], $words[4], 
           $words[6], 
           $words[10], $words[11]);  
  $lineno += 1;
}
&tablefin();

  
#Forecasts by valid point:
print outfile "<H3>Forecast verification by forecast point<\/H3>\n";
$line=<infile>;
$line=<infile>;
#$header[0] = "pt";
$header[1] = "Point";
#$header[2] = "day";
$header[3] = "Lead";
$header[4] = "Observations";
#$header[5] = "lead2";
#$header[6] = "Index of agreement";
$header[7] = "Scalar correlation";
$header[8] = "Vector correlation";
#$header[9] = "b0";
$header[10] = "b1";
#$header[11] = "Distance Bias";
#$header[12] = "Direction Bias";
#$header[13] = "RMS Distance";
$header[14] = "RMS Direction";
$header[15] = "Mean Error Radius";
#$header[16] = "RMS Error Radius";
#&tablestart(@header);
  print outfile "<TABLE BORDER=2>\n";
  print outfile "<TR>\n";
  print outfile "<TD>$header[1]<\/TD>\n";
  print outfile "<TD>$header[3]<\/TD>\n";
  print outfile "<TD>$header[4]<\/TD>\n";
  print outfile "<TD>$header[7]<\/TD>\n";
  print outfile "<TD>$header[8]<\/TD>\n";
  print outfile "<TD>$header[10]<\/TD>\n";
  print outfile "<TD>$header[14]<\/TD>\n";
  print outfile "<TD>$header[15]<\/TD>\n";
  print outfile "<\/TR>\n";

while ($line = <infile> ) {
  chop($line);
  @words = split(" ", $line);
  if ($#words < 9 ) {last;}
  &tabline($words[1], $words[3], $words[4], 
           $words[7], $words[8], 
           $words[10], $words[14], 
           $words[15]);  
  $lineno += 1;
}
&tablefin();

#print out html footer
print outfile "<HR>";
print outfile "<A HREF=\"stats.html\">Return to main drift statistics page<\/A><BR>\n";
print outfile "Last Modified 10 March 2014<BR>\n";
print outfile "<A HREF=\"mailto:Robert.Grumbine@noaa.gov\">Robert Grumbine<\/A>\n";
print outfile "\<\/BODY>\<\/HTML>\n";


#################################################################
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
#$header[0] = "Number of observations";
#$header[1] = "Forecast lead";
#$header[2] = "Index of agreement";
#$header[3] = "Scalar correlation";
#$header[4] = "Vector correlation";
#$header[5] = "b0";
#$header[6] = "b1";
#$header[7] = "Distance Bias";
#$header[8] = "Direction Bias";
#$header[9] = "RMS Distance";
#$header[10] = "RMS Direction";
#$header[11] = "Mean Error Radius";
#$header[12] = "RMS Error Radius";
  print outfile "<TR>\n";
  for ($i = 0; $i <= $#_; $i++) {
     print outfile "<TD align=\"center\">$_[$i]<\/TD>\n";
  }
  print outfile "<\/TR>\n";
}
