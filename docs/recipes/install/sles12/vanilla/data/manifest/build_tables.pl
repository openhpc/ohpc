#!/usr/bin/env perl

use warnings;
use strict;

my @fspCategories = ("admin","compiler-families","dev-tools");

foreach my $category (@fspCategories) {
    print "Building latex table for packages in the $category category...\n";

    my $filein="pkg-fsp.$category";
    open(IN,"<$filein")  || die "Cannot open file -> $filein\n";

    my $fileout="$category.tex";
    open(OUT,">$fileout")  || die "Cannot open file -> $fileout\n";

    # Table format/header

    print OUT "\\small\n";
    print OUT "\\begin{tabularx}{\\textwidth}{r|c|X}\n";
    print OUT "\\toprule\n";
    print OUT "{\\bf RPM Package Name} & {\\bf Version} & {\\bf Info/URL}  \\\\ \n";
    print OUT "\\midrule\n";
    print OUT "\n";

    while(<IN>) {

	# example format
	# pdsh-fsp 2.31 http://sourceforge.net/projects/pdsh fsp/admin Parallel remote shell program
	if($_ =~ /^(\S+) (\S+) (\S+) (fsp\/\S+) (.+)$/) {
	    my $name=$1;
	    my $version=$2;
	    my $url_raw=$3;
	    my $summary=$5;

	    # strip http(s) from url

	    my $url = $url_raw;

	    if( $url_raw =~ /http:\/\/(\S+)/) {
		$url=$1;
	    } elsif ( $url_raw =~ /https:\/\/(\S+)/) {
		$url=$1;
	    }

	    # latex entry

	    print OUT "% <-- begin entry for $name\n";
	    print OUT "\\multirow{2}{*}{$name} & \n";
	    print OUT "\\multirow{2}{*}{$version} & \n";
	    if ($url ne "(none)") {
		print OUT "$summary \\newline ";
		print OUT "($url) \n"
	    } else {
		print OUT "\\multirow{2}{*}{$summary} \\\\\n";
		print OUT "& & \n";
	   }
	    
	    print OUT "\\\\ \\hline \n";
	    print OUT "% <-- end entry for %name\n\n";


	}	    
    }

    print OUT "\\bottomrule\n";
    print OUT "\\end{tabularx}\n";

    close(IN);
    close(OUT);
}


