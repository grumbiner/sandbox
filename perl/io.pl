#!/usr/bin/perl

#Mac interface to find files:
use Mac::Files;
use Mac::StandardFile;

#Open an output file:
$outinfo = StandardPutFile("Enter a file name for output","defaultname");
$ret = FSpCreate($outinfo, "EDIT", "TEXT");
print $outinfo->sfFile, "\n";

open FOUT, ">$outinfo->sfFile" || die "failed to open output";

print "Hello\n";
print FOUT "Hello\n";
