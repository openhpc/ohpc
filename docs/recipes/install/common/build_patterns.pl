#!/usr/bin/env perl

use warnings;
use strict;


my $longSummaryLine = 60;
my $urlColor="blue";
my $REMOVE_HTTP=0;
my $FIXD_WIDTH=1;

my $filein="pattern-ohpc.all";
open(IN,"<$filein")  || die "Cannot open file -> $filein\n";

my $fileout="patterns.tex";
open(OUT,">$fileout")  || die "Cannot open file -> $fileout\n";

# Table format/header

print OUT "\\newcolumntype{C}[1]{>{\\centering}p{#1}} \n";
print OUT "\\newcolumntype{L}[1]{>{\\raggedleft}p{#1}} \n";

print OUT "\\small\n";
print OUT "\\begin{tabularx}{\\textwidth}{r|X}\n";
#print OUT "\\begin{tabularx}{\\textwidth}{L{\\firstColWidth{}}|C{\\secondColWidth{}}|X}\n";
print OUT "\\toprule\n";
print OUT "{\\bf Group Name} & {\\bf Description} \\\\ \n";
print OUT "\\midrule\n";
print OUT "\n";

# Read in data and cache

my @nameData    = ();
my @versionData = ();
my @urlData     = ();
my @summaryData = ();

while(<IN>) {
    
    # example format
    # ohpc-autotools Collection of GNU autotools packages

    if($_ =~ /^(\S+) (.+)$/) {
	my $name=$1;
	my $summary=$2;

	# Include period for summary
	    
	if( (substr $summary,-1) ne "." ) {
	    $summary = "$summary.";
	}

        # escape underscore

        $name =~ s/_/\\_/g;
        $summary =~ s/_/\\_/g;

        print "name = $name\n";

	print OUT "$name & $summary \\\\ \n";

    }
}

close(IN);

print OUT "\\bottomrule\n";
print OUT "\\end{tabularx}\n";
    
close(OUT);



