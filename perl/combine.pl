#!/usr/bin/perl

#read in summary of retrievals, and further summarize down to file level
while ($line = <STDIN>) {
  chop($line);
  @words = split(/\s+/, $line);
  #print $words[1]," ",$words[$#words]," ",&basename($words[$#words]),"\n";
  $totals{&basename($words[$#words])} += $words[1];
  $all += $words[1];
}

while ( ( $key, $value) = each(%totals) ) {
  print $value, " ", $key,"\n";
}
print "total count of transfers: ",$all,"\n";

sub basename {
  $last = $_[0];
  @fn = split(/\//, $last);
  $fn[$#fn];
}
# 1450 retrievals of /pub/history/sst/rtg_sst_grb_0.5.20031213
# 1387 retrievals of /pub/history/sst/rtg_sst_grb_0.5.20031014
# 1384 retrievals of /pub/history/sst/rtg_sst_grb_0.5.20031024
# 1077 retrievals of /pub/history/sst/rtg_sst_grb_0.5.20031215
# 1076 retrievals of /pub/history/sst/rtg_sst_grb_0.5.20031214
# 1065 retrievals of /pub/history/sst/rtg_sst_grb_0.5.20030225
# 1041 retrievals of /pub/fog/t00z.gvisg
#  968 retrievals of /pub/fog/t12z.gvisg
#  926 retrievals of /pub/history/sst/rtg_sst_grb_0.5.20031224
#  709 retrievals of /pub/history/waves/nww3.dp.200510.grb
