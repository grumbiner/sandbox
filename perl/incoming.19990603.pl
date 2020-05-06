#!/usr/bin/perl
#Use perl to construct a script that will take care of file management
#  for the ftp incoming directory.  This includes:
#Topex data /export/s2/altimeter 
#OTIS data  /export/s2/otis, /export/s2/otis.global
#XBT files  /export/s2/xbt
#Grouper files /export/s2/xbt/gs/pushed /export/s2/xbt/gs
#
#Robert Grumbine 9 April 1999
 
#Define destination directories and verify that they're present
$topex_base="/export/s2/altimeter/";
$topex_ps="/export/s2/altimeter/ps/";
$otis_base="/export/s2/otis/";
$otis_glob="/export/s2/otis.global/";
$xbt="/export/s2/xbt/";
$grouper_pull="/export/s2/xbt/gs/";
$grouper_push="/export/s2/xbt/gs/pushed/"; #Note that these arrive in different
                                          #directories than the rest.
@dests = ($topex_base, $topex_ps, $otis_base, $otis_glob, $xbt, $grouper_pull, $grouper_push);
foreach $j (@dests) {
  if ( -d $j ) {
    #print "We do have directory ",$j," \n";
  }
  else {
    die "No such directory ",$j," \n";
  }
}


#Construct a stop list so as to know who is normal and should be ignored
foreach $j ("/home/ftp/incoming/168","/home/ftp/incoming/OTIS",
            "/home/ftp/incoming/lost+found" ) {
  $stopl{$j} += 1;
}

#Note that this method only works for 1 pattern going to 1 destination.
$patterns{$topex_ps} = "topex_\\d\{8\}.ps";
$patterns{$topex_base} = "topex_\\d\{8\}";
#$patterns{$topex_ps} = "topex_d8.ps";
#$patterns{$topex_base} = "topex_d8";
$patterns{$otis_base} = "\[RTB\]\\d\{2\}X000GRB\.\\d\{3\}";
$patterns{$otis_glob} = "\[RTB\]\\d\{2\}1000GRB\.\\d\{3\}";
$patterns{$xbt} = "\\d\{8\}00\.profile";
@pattest=values(%patterns);

#Note that this check will miss patterns that match due to digit or char
#  expansions
for ($j = 0; $j <= $#pattest ; $j++) {
  for ($i = 0; $i <= $#pattest; $i++) {
     if ($i != $j) {
       #print $j, " ",$i," ", $pattest[$j]," ", $pattest[$i], "\n";
       if ($pattest[$j] =~ $pattest[$i] ) {
         printf "Pattern match for %d %d, %s %s\n",$j, $i, 
                           $pattest[$j], $pattest[$i];
       }
    }
  }
}

      

######################################################################
#Obtain the directory listing and then see who to do something with:
@a = </home/ftp/incoming/*>;
foreach $full_name (@a) {

  if ( &instop($full_name) ) {
    next;  #Go to next item if file is in the stoplist 
  }

  $count = 0;
  foreach $j (@pattest) {
    if ($full_name =~ $j) {
      print $full_name," matched ",$j," \n";
      $count += 1;
    }
  }

  if ($count == 1) {
    foreach $j (keys(%patterns)) {
      if ($full_name =~ $patterns{$j} ) {
        @fullpath = split("/",$full_name);
        $n = $#fullpath;
        $dest = $j.$fullpath[$n];
        system "mv $full_name $dest";
      }
    }
  }
  elsif ($count > 1) {
    print "Not currently handling multiple match cases, count = ",$count,"\n";
  }
  else {
    print "No match for ",$fullname,"\n";
  }

}



################################Subroutines
sub instop {
  local $tempor, $tword;
  $tword = $_[0];
  $tempor = $stopl{$tword};
  $tempor;
}
