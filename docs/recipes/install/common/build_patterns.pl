#!/usr/bin/env perl

use warnings;
use strict;


my $longSummaryLine = 60;
my $urlColor="blue";
my $REMOVE_HTTP=0;
my $FIXD_WIDTH=1;

# Define any packages that should start a new page (hash value determines which
# extra file to create) - per arch basis

my %page_breaks = ();
if ( $ENV{'PWD'} =~ /\S+\/x86_64\// ) {
    $page_breaks{"ohpc-intel-mvapich2-io-libs"} = 2;
}

sub write_table_header {
    my $fd = shift;

    # Table format/header

    print $fd "\\newcolumntype{C}[1]{>{\\centering}p{#1}} \n";
    print $fd "\\newcolumntype{L}[1]{>{\\raggedleft}p{#1}} \n";

    print $fd "\\small\n";
    print $fd "\\begin{tabularx}{\\textwidth}{r|X}\n";
    print $fd "\\toprule\n";
    print $fd "{\\bf Group Name} & {\\bf Description} \\\\ \n";
    print $fd "\\midrule\n";
    print $fd "\n";

}

my $filein="pattern-ohpc.all";
open(IN,"<$filein")  || die "Cannot open file -> $filein\n";

my $fileout="patterns.tex";
open my $OUT, '>', $fileout || die "Cannot open file -> $fileout\n";
#open(OUT,">$fileout")  || die "Cannot open file -> $fileout\n";

# Table format/header

write_table_header($OUT);

##print OUT "\\newcolumntype{C}[1]{>{\\centering}p{#1}} \n";
##print OUT "\\newcolumntype{L}[1]{>{\\raggedleft}p{#1}} \n";
##
##print OUT "\\small\n";
##print OUT "\\begin{tabularx}{\\textwidth}{r|X}\n";
##print OUT "\\toprule\n";
##print OUT "{\\bf Group Name} & {\\bf Description} \\\\ \n";
##print OUT "\\midrule\n";
##print OUT "\n";

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

	if ( exists $page_breaks{$name} ) {
	    print "<-- page break -->\n";
	    print $OUT "\\bottomrule\n";
	    print $OUT "\\end{tabularx}\n";
	    close($OUT);
	    my $filenew = "patterns" . "$page_breaks{$name}" . ".tex";
	    open $OUT, '>', $filenew || die "Cannot open file -> $filenew\n";
	    write_table_header($OUT)
	}

        print "name = $name\n";

	print $OUT "$name & $summary \\\\ \n";
	print $OUT "\\hline\n";

    }
}

close(IN);

print $OUT "\\bottomrule\n";
print $OUT "\\end{tabularx}\n";
    
close(OUT);



