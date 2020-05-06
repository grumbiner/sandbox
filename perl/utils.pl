sub tosecs {
  local $tmp1;
  @times=split(/:/, $_[0]);
  $tmp1=$times[0]*60 + $times[1];
  #print $#times," ",$tmp1," secs ",$times[0]," ", $times[1],"\n";
  $tmp1;
} 

sub max {
  if ( $_[1] > $_[0] ) {
    $_[1];
  }
  else {
    $_[0];
  }
}
