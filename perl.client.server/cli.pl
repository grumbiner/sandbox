#!/usr/bin/perl -w

require 5.002;

use strict;
use Socket;

my($remote, $port, $iaddr, $paddr, $proto, $line);

$remote = shift || 'localhost';
$port = shift || 4843;
if ($port =~ /\D/) { $port = getservbyname($port, 'tcp') }
die "No port" unless $port;
$iaddr = inet_aton($remote)  or die "no host: $remote";
$paddr = sockaddr_in($port, $iaddr);

$proto = getprotobyname('tcp');

socket(SOCK, PF_INET, SOCK_STREAM, $proto) or die "socket: $!";
connect (SOCK, $paddr) or die "connect: $!";

while ($line = <SOCK>) {
  print $line;
}

close (SOCK) or die "close: $!";
exit;


