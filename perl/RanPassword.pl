#!/usr/bin/perl

## Random Password Generator (Perl)
## Copyright 1998 Paul Schreiber
## paul@magic.ca
## http://paulschreiber.com/
## version 1.4
## November 22, 1998
## This is completely free! No warranty!
## Commercial Distribution requires permission!

## character settings
$charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
$charsetLength = length ($charset);
$returnChar = "\n";

## default values
$howManyTimes = 5;
$howManyLetters = 6;
$passFile = "PasswordList";

## check the number of command-line arguments
$numArgs = scalar(@ARGV);

if (($numArgs != 0) && ($numArgs != 3)) {
	print "Usage: ./RanPassword.pl [passwords length filename]\n";
	print "- passwords is the number of passwords desired\n";
	print "- length is the length of each password\n";
	print "- filename is the file where the passwords are saved\n";
	exit (-1);
}

## get the command-line args
if ($numArgs == 3) {
	$howManyTimes = $ARGV[0];
	$howManyLetters = $ARGV[1];
	$passFile = $ARGV[2];
}

## check if the number of passwords is an integer
if ($howManyTimes =~ /[^0-9]/) {
	print "You must specify a valid number of passwords.\n";
	exit (-1);
}

## check if the length of passwords is an integer
if ($howManyLetters =~ /[^0-9]/) {
	print "You must specify a valid password length.\n";
	exit (-1);
}

## check if the file already exists (don't overwrite)
if (-e $passFile) {
	print "The file \"$passFile\" already exists.\n";
	exit (-1);
}

## check if the current directory is writeable
if (! -w ".") {
	print "The current directory is not writeable.\n";
	exit (-1);
}

## seed the random number generator
srand();

open (PASSFILE,">$passFile");

for (my $i = 0; $i < $howManyTimes; $i++) {
	$thePassword = "";
	for (my $theLetters = 0; $theLetters < $howManyLetters; $theLetters++) {
		$ranString = rand($charsetLength);
		$ranNum = substr($ranString, 0, index($ranString, "."));
		$thePassword .= substr($charset, $ranNum, 1);
	}
	
	print PASSFILE $thePassword,$returnChar;
}

close (PASSFILE);
