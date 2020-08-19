#!/usr/bin/perl

#parse the maillog, looking for abusive senders
#Nov  7 05:45:21 polar sendmail[27011]: FAA27011: from=<kabo2020@lycos.com>, size=8157, class=-30, pri=92157, nrcpts=1, msgid=<200211071045.FAA27011@polar.ncep.noaa.gov>, proto=ESMTP, relay=loft.wwb.noaa.gov [140.90.193.45]

$fin = $ARGV[0];
open fin, "<$ARGV[0]";

while ($line = <fin>) {
  chop($line);
  if ($line =~ /from\=\<(\S+)\@(\S+)\>/ ) {
    $sender = $1 . "@" . $2;
    $count{$sender} += 1;
  }
}

while ( ($key, $value) = each(%count) ) {
  print $value, " ", $key,"\n";
}
