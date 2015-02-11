#!/usr/bin/env perl

use warnings;
use strict;

my @fspCategories    = ("admin","compiler-families","dev-tools","distro-packages","io-libs","lustre","mpi-families",
                        "lustre","parallel-libs","serial-libs","perf-tools","provisioning","rms");
my @compiler_familes = ("gnu","intel");
my @mpi_families     = ("mvapich2","openmpi","impi");

my @single_package_exceptions = ("lmod-defaults-intel-fsp");

foreach my $category (@fspCategories) {
    print "Building latex table for packages in the $category category...\n";

    my $filein="pkg-fsp.$category";
    open(IN,"<$filein")  || die "Cannot open file -> $filein\n";

    my $fileout="$category.tex";
    open(OUT,">$fileout")  || die "Cannot open file -> $fileout\n";

    # Table format/header

    print OUT "\\small\n";
    print OUT "\\begin{tabularx}{\\textwidth}{l|c|X}\n";
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

	    if( $url_raw =~ /http:\/\/(\S+)/) {
		$url=$1;
	    } elsif ( $url_raw =~ /https:\/\/(\S+)/) {
		$url=$1;
	    }

	    # Include period for summary
	    
	    if( (substr $summary,-1) ne "." ) {
		$summary = "$summary.";
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
    while ($i < $#nameData) {
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
		print OUT "$nameData[$k] & \n";
		if($k == $i) {
		    print OUT "\\multirow{$delta}{*}{$versionData[$k]} & \n";
#		    die unless ("$urlData[$k]" ne "(none)");
		    print OUT "\\multirow{$delta}{\\linewidth}{$summaryData[$k] \\newline ($urlData[$k])} \\\\ \n";
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
	    
 	    print OUT "\\multirow{2}{*}{$name_base} & \n";
 	    print OUT "\\multirow{2}{*}{$versionData[$i]} & \n";
 	    if ($urlData[$i] ne "(none)") {
 		print OUT "$summaryData[$i] \\newline ";
 		print OUT "($urlData[$i]) \n"
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
    
    close(OUT);
}


