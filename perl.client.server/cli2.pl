#!/usr/bin/perl -w

require 5.002;

use strict;
use Socket;

my($remote, $port, $iaddr, $paddr, $proto, $line);
my ($i, @words) ;

$remote = shift || 'localhost';
$port = shift || 4843;
if ($port =~ /\D/) { $port = getservbyname($port, 'tcp') }
die "No port" unless $port;
$iaddr = inet_aton($remote)  or die "no host: $remote";
$paddr = sockaddr_in($port, $iaddr);

$proto = getprotobyname('tcp');

#Loop to open a socket every 5 seconds and see what's available
while (1==1) {
  sleep 5;

  socket(SOCK, PF_INET, SOCK_STREAM, $proto) or die "socket: $!";
  connect (SOCK, $paddr) or die "connect: $!";

  while ($line = <SOCK>) {
    if (! ($line =~ /total/i) ) {
      #print $line;
      @words=split(/\s+/, $line);
      $line =~ /(\S+\s+)+/;
      print $words[10],"\n";
    
    }
  }

  close (SOCK) or die "close: $!";
}
