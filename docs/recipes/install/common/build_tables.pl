#!/usr/bin/env perl

use warnings;
use strict;

my @fspCategories    = ("admin","compiler-families","dev-tools","distro-packages","io-libs","lustre","mpi-families",
                        "lustre","parallel-libs","serial-libs","perf-tools","provisioning","rms");
my @compiler_familes = ("gnu","intel");
my @mpi_families     = ("mvapich2","openmpi","impi");

my @single_package_exceptions = ("lmod-defaults-intel-fsp","mkl-blacs-gnu-openmpi-fsp");

my $longSummaryLine = 60;
my $urlColor="blue";
my $REMOVE_HTTP=0;
my $FIXD_WIDTH=1;

foreach my $category (@fspCategories) {
    print "Building latex table for packages in the $category category...\n";

    my $filein="pkg-fsp.$category";
    open(IN,"<$filein")  || die "Cannot open file -> $filein\n";

    my $fileout="$category.tex";
    open(OUT,">$fileout")  || die "Cannot open file -> $fileout\n";

    # Table format/header

    print OUT "\\newcolumntype{C}[1]{>{\\centering}p{#1}} \n";
    print OUT "\\newcolumntype{L}[1]{>{\\raggedleft}p{#1}} \n";

#    print OUT "\\footnotesize\n";
    print OUT "\\small\n";
#    print OUT "\\begin{tabularx}{\\textwidth}{c|c|X}\n";
#    print OUT "\\begin{tabularx}{\\textwidth}{C{4.5cm}|c|X}\n";
    print OUT "\\begin{tabularx}{\\textwidth}{L{\\firstColWidth{}}|C{\\secondColWidth{}}|X}\n";
    print OUT "\\toprule\n";
    print OUT "{\\bf RPM Package Name} & {\\bf Version} & {\\bf Info/URL}  \\\\ \n";
    print OUT "\\midrule\n";
    print OUT "\n";

    # Read in data and cache

    my @nameData    = ();
    my @versionData = ();
    my @urlData     = ();
    my @summaryData = ();
    
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

	    if($REMOVE_HTTP) {
		if( $url_raw =~ /http:\/\/(\S+)/) {
		    $url=$1;
		} elsif ( $url_raw =~ /https:\/\/(\S+)/) {
		    $url=$1;
		}
	    }

	    # Include period for summary
	    
	    if( (substr $summary,-1) ne "." ) {
		$summary = "$summary.";
	    }

	    # trim final slash from url

	    if ($url =~/(.*)\/$/) {
		$url = $1;
	    }

	    push(@nameData,   $name);
	    push(@versionData,$version);
	    push(@urlData,    $url);
	    push(@summaryData,$summary);
	}
    }

    close(IN);

    # Loop over cached data and generate latex data

    my $i = 0;
    while ($i <= $#nameData) {
	my $name = $nameData[$i];

	print "working on package $name\n";

	# Check if this is a compiler/MPI family package

	my $compiler_package = 0;
	my $mpi_package      = 0;
	my $name_base        = $name;

	foreach my $compiler (@compiler_familes) {
	    foreach my $mpi (@mpi_families) {
		if($name =~ /(\S+)-$compiler-fsp/) {
		    die "unknown package family for compiler" unless ($mpi_package == 0);
		    $compiler_package = 1;
		    $name_base = $1;
		} elsif ($name =~ /(\S+)-$compiler-$mpi-fsp/) {
		    die "unknown package family for MPI" unless ($compiler_package == 0);
		    $mpi_package = 1;
		    $name_base = $1;
		}
	    }
	}

	# Verify we don't have an exception

	if( $compiler_package || $mpi_package ) {
	    foreach my $exception (@single_package_exceptions) {
		if($name eq $exception) {
		    print "single package exception for $name\n";
		    $compiler_package=0;
		    $mpi_package=0;
		}
	    }
	}

	print "   --> compiler_package = $compiler_package\n";
	print "   --> mpi_package      = $mpi_package\n";
		
	# latex entry

 	print OUT "% <-- begin entry for $name_base\n";
	
	if($compiler_package == 1 || $mpi_package == 1) {
	    my $end_index = $i;

	    # Find out how many packages in this family
	    for my $j ($i .. $#nameData) {
		if( $compiler_package && $nameData[$j] =~ /$name_base-(\S+)-fsp$/) { 
		    $end_index = $j;
		} elsif ($mpi_package && $nameData[$j] =~ /$name_base-(\S+)-(\S+)-fsp$/) {
		    $end_index = $j;
		} else {
		    last;
		}
	    }

	    my $delta = $end_index - $i + 1;
	    print "number of package versions in family = $delta ($name_base)\n";

	    die "unexpected # of compiler families for $name_base" if ($compiler_package && ($delta != 2));
	    die "unexpected # of mpi families for $name_base" if ($mpi_package && ($delta != 6));

	    my $k = $i;
	    for my $k ($i .. $end_index) {

                my $lname = $nameData[$k];
                $lname =~ s/_/\\_/g;

		print OUT "$lname & \n";

		if($k == $i) {
		    my $sumLength = length($summaryData[$k]);

		    print OUT "\\multirow{$delta}{*}{$versionData[$k]} & \n";
		    print OUT "\\multirow{$delta}{\\linewidth}{$summaryData[$k] ";
		    if($sumLength <= $longSummaryLine || $delta > 2) {
			print OUT "\\newline";
		    }
		    print OUT " {\\color{$urlColor} $urlData[$k]}} \\\\ \n";
		} else {
		    print OUT "& \\\\ \n";
	        }
	    }
	    print OUT "\\hline\n";
	    # skip to next package
	    $i = $end_index+1;
	    print "skipping\n"; 
	    print OUT "% <-- end entry for $name_base\n\n";
	    next;
	} else {

	    my $sumLength = length($summaryData[$i]);

	    my $lname = $name_base;
	    $lname =~ s/_/\\_/g;

	    
 	    print OUT "\\multirow{2}{*}{$lname} & \n";
 	    print OUT "\\multirow{2}{*}{$versionData[$i]} & \n";
 	    if ($urlData[$i] ne "(none)") {
 		print OUT "$summaryData[$i] ";
		if($sumLength <= $longSummaryLine) {
		    print OUT "\\newline";
		}
 		print OUT " { \\color{$urlColor} $urlData[$i]} \n"
 	    } else {
 		print OUT "\\multirow{2}{*}{$summaryData[$i]} \\\\\n";
 		print OUT "& & \n";
 	    }
 	    
 	    print OUT "\\\\ \\hline \n";
 	    print OUT "% <-- end entry for %name_base\n\n";
 	}

	$i++;
    }	    
    
    print OUT "\\bottomrule\n";
    print OUT "\\end{tabularx}\n";
#    print OUT "\\end{tabular}\n";
    
    close(OUT);
}


