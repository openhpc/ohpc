#!/usr/bin/env perl

use warnings;
use strict;
use Getopt::Long;
use Cwd;

sub usage {
    print "\n";
    print "Usage: build_tables.pl [OPTIONS]\n\n";
    print "  where available OPTIONS are as follows:\n\n";
    print "     -h --help                      generate help message and exit\n";
    print "        --category [name]           update provided category table only\n";
    print "\n";

    exit(0);
}

my @ohpcCategories    = ("admin","compiler-families","dev-tools","distro-packages","io-libs","mpi-families",
                        "parallel-libs","serial-libs","perf-tools","provisioning","rms", "runtimes");
my @compiler_familes = ("gnu","gnu7","intel","gnu8","gnu9","gnu12","gnu13","gnu14","arm1");
my @mpi_families     = ("mvapich2","openmpi","openmpi3","openmpi4","openmpi5","impi","mpich");

my @single_package_exceptions = ();
my @exclude = ("slurm-sjstat-ohpc","slurm-slurmdb-direct-ohpc","slurm-sjobexit-ohpc","slurm-sql-ohpc",
               "slurm-munge-ohpc","slurm-plugins-ohpc");  # package name changes with slurm 17.02.9
push @exclude, "lustre-client-ohpc-kmp-default";
push @exclude, "R_base-ohpc";

## # include lustre for x86
## if ( $ENV{'PWD'} =~ /\S+\/x86_64\// ) {
##     push @ohpcCategories, "lustre";
## }


# skip older lmod defaults
push @exclude, "lmod-defaults-gnu-impi-ohpc";
push @exclude, "lmod-defaults-gnu-mpich-ohpc";
push @exclude, "lmod-defaults-gnu-mvapich2-ohpc";
push @exclude, "lmod-defaults-gnu-openmpi-ohpc";
if ( $ENV{'PWD'} =~ /\S+\/x86_64\// ) {
    push @exclude, "lmod-defaults-arm1-mpich-ohpc";
    push @exclude, "lmod-defaults-arm1-openmpi4-ohpc";
}

my $help;
my $category_single;

GetOptions("h"          => \$help,
           "category=s" => \$category_single ) || usage();

if($help) {usage()};
if($category_single) {
    print "--> updating table contents for $category_single only\n";
    @ohpcCategories = $category_single;
}

# Define any asymmetric compiler packages

my %compiler_exceptions = ();
$compiler_exceptions{"gsl"} = 1;
$compiler_exceptions{"openblas"} = 1;

$compiler_exceptions{"mvapich2"} = 4;
$compiler_exceptions{"openmpi"} = 4;
$compiler_exceptions{"openmpi3"} = 4;

# Define any asymmetric MPI packages
my %mpi_exceptions = ();
$mpi_exceptions{"python-scipy"} = 3;
$mpi_exceptions{"fftw"} = 3;
$mpi_exceptions{"mkl-blacs"} = 1;

# Define any packages that should start a new page (hash value determines which
# extra file to create) - per arch basis

my %page_breaks = ();
if ( $ENV{'PWD'} =~ /\S+\/x86_64\// ) {
    $page_breaks{"mpiP-gnu-impi-ohpc"} = 2;
    $page_breaks{"pdtoolkit-gnu14-ohpc"} = 2;
#    $page_breaks{"pdtoolkit-gnu13-ohpc"} = 3;
    $page_breaks{"pnetcdf-gnu14-impi-ohpc"} = 2;
    $page_breaks{"mumps-gnu14-impi-ohpc"} = 2;
    $page_breaks{"superlu_dist-gnu14-impi-ohpc"} = 3;
} elsif ( $ENV{'PWD'} =~ /\S+\/aarch64\// ) {
    $page_breaks{"scalapack-gnu9-mpich-ohpc"} = 2;
}

my $longSummaryLine = 60;
my $urlColor="logoblue";
my $REMOVE_HTTP=0;
my $FIXD_WIDTH=1;

# Determine arch specific settings
my $pwd = getcwd;
my $numCompiler_permute;
my $numMPI_permute;

if ( $pwd =~ /\/x86_64\// ) {
    print "arch = x86_64\n";
    $numCompiler_permute = 2;
    $numMPI_permute = 8;
} elsif ( $pwd =~ /\/aarch64\// ) {
    print "arch = aarch64\n";
    $numCompiler_permute = 1;
    $numMPI_permute = 3;
} else {
    die ("Unable to determine architecture from local path ($pwd)")
}

sub write_table_header {
    my $fd = shift;

    # Table format/header

    print $fd "\\newcolumntype{C}[1]{>{\\centering}p{#1}}\n";
    print $fd "\\newcolumntype{L}[1]{>{\\raggedleft}p{#1}}\n";

    print $fd "\\small\n";
    print $fd "\\begin{tabularx}{\\textwidth}{L{\\firstColWidth{}}|C{\\secondColWidth{}}|X}\n";
    print $fd "\\toprule\n";
    print $fd "{\\bf RPM Package Name} & {\\bf Version} & {\\bf Info/URL}  \\\\\n";
    print $fd "\\midrule\n";
    print $fd "\n";

}

foreach my $category (@ohpcCategories) {
    print "Building latex table for packages in the $category category...\n";

    my $filein="pkg-ohpc.$category";
    open(IN,"<$filein")  || die "Cannot open file -> $filein\n";

    my $fileout="$category.tex";
    open my $OUT, '>', $fileout || die "Cannot open file -> $fileout\n";

    # Table format/header

    print $OUT "\\newcolumntype{C}[1]{>{\\centering}p{#1}}\n";
    print $OUT "\\newcolumntype{L}[1]{>{\\raggedleft}p{#1}}\n";

    write_table_header($OUT);

    # Read in data and cache

    my @nameData    = ();
    my @versionData = ();
    my @urlData     = ();
    my @summaryData = ();

    while(<IN>) {

	# example format
	# pdsh-ohpc 2.31 https://github.com/chaos/pdsh ohpc/admin Parallel remote shell program
	if($_ =~ /^(\S+) (\S+) (\S+) (ohpc\/\S+) (.+)$/) {
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

	    # Skip any excluded packages
	    if ( grep ( /$name$/, @exclude) ) {
		print "--> skipping $name per exclude request\n";
		next;
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
		if($name =~ /(\S+)-$compiler-ohpc/) {
		    die "unknown package family for compiler" unless ($mpi_package == 0);
		    $compiler_package = 1;
		    $name_base = $1;
		} elsif ($name =~ /(\S+)-$compiler-$mpi-ohpc/) {
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

 	print $OUT "% <-- begin entry for $name_base\n";

	if($compiler_package == 1 || $mpi_package == 1) {
	    my $end_index = $i;

	    # Find out how many packages in this family
	    for my $j ($i .. $#nameData) {
		if( $compiler_package && $nameData[$j] =~ /$name_base-(\S+)-ohpc$/) {
		    $end_index = $j;
		} elsif ($mpi_package && $nameData[$j] =~ /$name_base-(\S+)-(\S+)-ohpc$/) {
		    $end_index = $j;
		} else {
		    last;
		}
	    }

	    my $delta = $end_index - $i + 1;
	    print "number of package versions in family = $delta ($name_base)\n";

###             if($compiler_package) {
###                 if( exists $compiler_exceptions{$name_base} ) {
###                     die "unexpected # of compiler families for exception -> $name_base" if ($delta != $compiler_exceptions{$name_base});
###                 } else {
###                     die "unexpected # of compiler families for $name_base" if ( $delta != $numCompiler_permute) ;
###                 }
###             }
###
###             if ($mpi_package) {
###                 if( exists $mpi_exceptions{$name_base} ) {
###                     die "unexpected # of mpi families for exception -> $name_base" if ($delta != $mpi_exceptions{$name_base});
###                 } else {
###                     die "unexpected # of mpi families for $name_base" if ( $delta != $numMPI_permute );
###                 }
###             }

	    # Check if all versions are equal, compiler/mpi variant additions
	    # might introduce asymmetry that needs to be handled

	    my $versions_equal=1;
	    my $startVer=$versionData[$i];
	    my @verIndex = ();
	    for my $kk ($i+1 .. $end_index) {
		if ($versionData[$kk] ne $startVer ) {
		    print "versions do not match for $nameData[$kk] -> $startVer,$versionData[$kk]\n";
		    $versions_equal = 0;
		    push(@verIndex,$kk); # index at end of current version
		    $startVer = $versionData[$kk];
		}
	    }

	    if(!$versions_equal) {
		push(@verIndex,$end_index);
	    }

	    my $k = $i;
	    for my $k ($i .. $end_index) {

		# setup write to new file if page break package specified
		if( exists $page_breaks{$nameData[$k]} ) {
		    print $OUT "\\bottomrule\n";
		    print $OUT "\\end{tabularx}\n";
		    close($OUT);
		    my $filenew = "$category" . "$page_breaks{$nameData[$k]}" . ".tex";
		    open $OUT, '>', $filenew || die "Cannot open file -> $filenew\n";
		    write_table_header($OUT)
		}

                my $lname = $nameData[$k];
                $lname =~ s/_/\\_/g;

		if(! $versions_equal && ($k == $verIndex[0]) && (@verIndex > 1) ) {
		    print $OUT "\\cline{1-2} ";
		}

		print $OUT "$lname &";

		if(! $versions_equal && ($k == $verIndex[0]) && (@verIndex > 1) ) {
		    my $local_delta = $verIndex[1] - $verIndex[0] + 1;
		    shift @verIndex;
		    print $OUT "\\multirow{$local_delta}{*}{$versionData[$k]}";
		}
		print $OUT "\n";


		if($k == $i) {
		    my $sumLength = length($summaryData[$k]);
		    if($versions_equal) {
			print $OUT "\\multirow{$delta}{*}{$versionData[$k]} &\n";
		    } else {
			my $local_delta = $verIndex[0] - $i;
			print $OUT "\\multirow{$local_delta}{*}{$versionData[$k]} &\n";
		    }
		    print $OUT "\\multirow{$delta}{\\linewidth}{$summaryData[$k] ";
		    if($sumLength <= $longSummaryLine || $delta > 2) {
			print $OUT "\\newline";
		    }
		    print $OUT " {\\color{$urlColor} \\url{$urlData[$k]}}}\\\\\n"
		} else {
		    if($versions_equal) {
			print $OUT "& \\\\\n";
		    } else {
			print $OUT "& \\\\";
			print $OUT "\n";
		    }
	        }
	    }
	    print $OUT "\\hline\n";
	    # skip to next package
	    $i = $end_index+1;
	    print "skipping\n";
	    print $OUT "% <-- end entry for $name_base\n\n";
	    next;
	} else {

	    my $sumLength = length($summaryData[$i]);

	    my $lname = $name_base;
	    $lname =~ s/_/\\_/g;


 	    print $OUT "\\multirow{2}{*}{$lname} &\n";
 	    print $OUT "\\multirow{2}{*}{$versionData[$i]} &\n";
	    my $lsummary = $summaryData[$i];
	    $lsummary =~ s/_/\\_/g;
 	    if ($urlData[$i] ne "(none)") {
		print $OUT "$lsummary ";
		if($sumLength <= $longSummaryLine) {
		    print $OUT "\\newline";
		}
 		print $OUT " { \\color{$urlColor} \\url{$urlData[$i]}}\n"
 	    } else {
		print $OUT "\\multirow{2}{*}{$lsummary}\\\\\n";
		print $OUT "& &\n";
 	    }

 	    print $OUT "\\\\ \\hline\n";
 	    print $OUT "% <-- end entry for $name_base\n\n";
 	}

	$i++;
    }

    print $OUT "\\bottomrule\n";
    print $OUT "\\end{tabularx}\n";

    close($OUT);
}
