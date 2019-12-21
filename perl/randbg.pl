#!/usr/bin/perl -w -p -i.orig
# asetroot.pl - randomized backgrounds for AfterStep
# 0.1 by Chris Hilton, chris@dctank.com

BEGIN { push(@ARGV, ($ENV{HOME} || $ENV{LOGDIR} || (getpwuid($>))[7]) .  "/GNUstep/Library/AfterStep/asetroot"); }

if ( m#^MyBackground\s+\"([^"]*)\"# ) { push(@bgs, $1); }

if ( m#^\*asetrootDeskBack# ) {
	s#(?<=\")[^"]*(?=\")#splice(@bgs, rand @bgs, 1)#e;
}

