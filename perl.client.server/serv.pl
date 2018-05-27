#!/usr/bin/perl -Tw

require 5.002;
use strict;
BEGIN { $ENV{PATH} = '/usr/ucb:/bin' }
use Socket;
use Carp;

sub logmsg{print "$0 $$: @_ at ", scalar localtime, "\n" }

my $port = shift || 4843;
my $proto = getprotobyname('tcp');
socket (Server, PF_INET, SOCK_STREAM, $proto) or die "socket: $!";
setsockopt(Server, SOL_SOCKET, SO_REUSEADDR, pack("l",1)) 
                or die "setsockopt: $1";
bind(Server, sockaddr_in($port, INADDR_ANY)) or die "bind: $!";
listen(Server, SOMAXCONN) or die "listen: $!";

logmsg "server started on port $port";

my $paddr;
my $line;

$SIG{CHLD} = \&REAPER;


for ( ; $paddr = accept(Client, Server); close Client) {
  my ($port, $iaddr) = sockaddr_in($paddr);
  my $name = gethostbyaddr($iaddr, AF_INET);

  logmsg "connection from $name [",
  inet_ntoa($iaddr), "] at port $port";

  #print Client "Hello there, $name, it is now ", scalar localtime, "\n";

  #system "ls -ls > alpha";
  open(FIN, "<alpha");
  while ($line = <FIN>) {
    print Client $line;
    #print $line;
  }  
  close FIN;

}
