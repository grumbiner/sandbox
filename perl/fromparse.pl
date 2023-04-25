#!/usr/bin/perl

while ($line = <STDIN>) {
  #if ($line =~ /^From \S+\s+\S+(\S{3})\s+(\S+)/) {
  if ($line =~ /^From \S+\s+\S+\s+(\S+)\s+(\S+) (\S+:\S{2}:\S{2}|\S+:\S{2} E.T)(\s+| GMT )(\d+)/) {
    $tmon = $1;
    $mon = &recode($tmon);
    $tday = $2;
    $ttime = $3;
    $year = $5;
    if (length $tday < 2) {
      $day = "0" . $tday;
    }
    else {
      $day = $tday;
    }
     
    if ($ttime =~ /E/) {
      $ttime =~ /(\S+:\S{2})/ ;
      $time = $1;
      #print "time ",$time,"\n";
    }
    else {
      $time = $ttime;
    }
 
    #print $year, " ", $mon," ",$day," ", $time,"\n";
    $fout = $year . $mon . $day . "_" . $time;
    print $fout,"\n";
    open fout , ">$year$mon$day_$time";
    print fout $fout,"\n";
    close fout;
  }
  else {
    print "not: ",$line;
  }
}

#Transfer verbal month to numeric:
sub recode {
  local $tmp1, $work;
  #print $_[0],"\n";
  $work = $_[0];
  SWITCH: {
    if ($work =~ /Jan/) { $tmp1 = "01" ; last SWITCH; }
    if ($work =~ /Feb/) { $tmp1 = "02" ; last SWITCH; }
    if ($work =~ /Mar/) { $tmp1 = "03" ; last SWITCH; }
    if ($work =~ /Apr/) { $tmp1 = "04" ; last SWITCH; }
    if ($work =~ /May/) { $tmp1 = "05" ; last SWITCH; }
    if ($work =~ /Jun/) { $tmp1 = "06" ; last SWITCH; }
    if ($work =~ /Jul/) { $tmp1 = "07" ; last SWITCH; }
    if ($work =~ /Aug/) { $tmp1 = "08" ; last SWITCH; }
    if ($work =~ /Sep/) { $tmp1 = "09" ; last SWITCH; }
    if ($work =~ /Oct/) { $tmp1 = "10" ; last SWITCH; }
    if ($work =~ /Nov/) { $tmp1 = "11" ; last SWITCH; }
    if ($work =~ /Dec/) { $tmp1 = "12" ; last SWITCH; }
    $tmp1 = "not";
  }
  $tmp1;
}
