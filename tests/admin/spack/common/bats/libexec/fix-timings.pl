#!/usr/bin/perl

use strict;
use warnings;

use Getopt::Std;
use vars qw( $opt_i );

if( ! getopts('i:') || ! defined $opt_i)
{
  die "Usage: $0 -i <filename>\n";
}

my $time_count = 0;
my @durations;

open INPUT, "<$opt_i";

my @input = <INPUT>;

close INPUT;

my $skipflag;

for (@input) {
	if (/\<failure type/) { $skipflag = 1; }
	if (/failure>/ ) { undef $skipflag; }
	next if defined $skipflag;
	$time_count++ if (/time="0"/);
	if (/duration_ms: ([0-9]+\.[0-9]+)/) {
		push @durations, $1;
	}
	# if there was an error there will be duration lines in the stanrd-out/standard-error sections that duplicates the previous input
	# last if (/<system-out>/);
}

# 0 base time_count like arrays, and add one to the durations count for the total time we're going to prepend
if ($time_count-1 ne $#durations+1) {
	my $duration_count=$#durations+1;
	die "$0: Number of time lines[$time_count] (excluding initial total time line) doesn't match number of durations lines[$duration_count]\n";
}

my $total_duration = 0;
$total_duration += $_ for @durations;

unshift @durations, $total_duration;

for (@input) {
	if (/time="0"/) {
		my $duration = shift @durations;
		s/time="0"/time="$duration"/;
	}
	# last if (/<system-out>/);
}

open OUTPUT, ">$opt_i";

for (@input) { print OUTPUT $_; }

close OUTPUT;
