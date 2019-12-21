#!/usr/bin/perl

#Script to search out and collect e-mail addresses from
#  mail
#-- variant to come: pull up URLs

while ($line = <STDIN>) {
  chop($line);
  @words = split(/\s+/, $line);
  foreach $w (@words) {
     #For now we'll call it an address if it has an @.  Much to be cleared out. 
     if ($w =~ /@/) {
       if ( $w =~ /[<>"'`();,]/) {
       #$_[0] =~ s/[,;<>()"']+//;
         @parts = split(/@/, $w);
         $parts[0] = &clearup($parts[0]);
         $parts[1] = &clearup($parts[1]);
         #Following is to avoid including mime attachments.  Ick
         if ($parts[0] =~ /^[M|!|#|%|@][!|%|=|\`|$|#|\@|_|-|+|:|&|^]/ 
             || !($parts[1] =~ /@/)  # addresses cannot have multiple @
           ) {
           #skip
         }
         else {
           $w = join("@",@parts);
           $addrs{$w} += 1;
         }
       }
       else {
         #print  $w , "\n";
         $addrs{$w} += 1;
       }
     }
  }
}

while ( ($key, $value) = each(%addrs) ) {
  print $value," ", $key," \n";
  @pieces = split("@",$key);
  @doms   = split('\.',$pieces[1]);
  $x = $#doms;
  #print $pieces[1],"\n";
  #print  $x,"0 ", $doms[$x], " 1 ",$doms[$x - 1], " 2 ",$doms[$x-2], "\n";
  $last{$doms[$#doms ]} += $value;
}

print "Domain frequencies\n";
while (( $key, $value) = each(%last) ) {
  print $value, " ", $key,"\n";
}


sub clearup {
  local $tmp;
  $_[0] =~ s/[,;<>()"'`]+//;
  return $_[0];
}
