#!/usr/bin/perl
#Perl to process, with a.sh, the process file and look for especially
#  old (therefore likely hung/dead) ftp connections
#Robert Grumbine 8 June 2001

$procno = $ARGV[0];
$action = $ARGV[1];

#if ( $action =~ /clobber/ ) {
#  print "Would try to kill hung processes\n";
#}

while ($line = <stdin> ) {
  if ($line =~ /IDLE/) {
    @words = split(" ",$line);
    $tno = $words[0];
    if ($procno - $tno > 5000 ) {
      print "could kill idle ",$tno, " from ",$procno,"\n";
      if ($action =~ /clobber/) {
        system "kill $tno";
      }
    }
    elsif  ($procno - $tno < 0 && $procno - $tno > -(32768 - 5000) ) {
      print "could kill idle ",$tno, " from ",$procno,"\n";
      if ($action =~ /clobber/) {
        system "kill $tno";
      }
    }
    else {
      #print "not idle ",$tno," from ",$procno,"\n";
    }
  }
  if ($line =~ /NLST/) {
    @words = split(" ",$line);
    $tno = $words[0];
    if ($procno - $tno > 5000 ) {
      print "could kill nlst ",$tno, " from ",$procno,"\n";
      if ($action =~ /clobber/) {
        system "kill $tno";
      }
    }
    elsif  ($procno - $tno < 0 && $procno - $tno > -(32768 - 5000) ) {
      print "could kill nlst ",$tno, " from ",$procno,"\n";
      if ($action =~ /clobber/) {
        system "kill $tno";
      }
    }
    else {
      #print "not nlst ",$tno," from ",$procno,"\n";
    }
  }

  if ($line =~ /RETR/ ) {
    #print "test ",$line,"\n";
    @words = split(" ",$line);
    $tno = $words[0];
    if ($procno - $tno > 5000 ) {
      print "could kill RETR ",$tno, " from ",$procno,"\n";
      if ($action =~ /clobber/  && ($line =~ /bull|gif/ || $line =~ /gif/)  ) {
        system "kill $tno";
      }
    }
    elsif  ($procno - $tno < 0 && $procno - $tno > -(32768 - 5000) ) {
      print "could kill RETR ",$tno, " from ",$procno,"\n";
      if ($action =~ /clobber/  && ($line =~ /bull|gif/ || $line =~ /gif/)  ) {
        system "kill $tno";
      }
    }
  }

}
