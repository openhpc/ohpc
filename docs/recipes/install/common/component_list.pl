#!/usr/bin/env perl

use warnings;
use strict;
use Getopt::Long;
use Cwd;
use Sort::Versions;

sub usage {
    print "\n";
    print "Usage: component_list.pl [OPTIONS]\n\n";
    print "  where available OPTIONS are as follows:\n\n";
    print "     -h --help                      generate help message and exit\n";
    print "        --version  [version]        desired version to analyze\n";
    print "        --category [name]           update provided category table only\n";
    print "        --package  [plist]          add hard-coded package (category|name|url) format\n";
    print "\n";
    
    exit(0);
}

my @ohpcCategories       = ("admin","compiler-families","dev-tools","io-libs","lustre","mpi-families",
			    "parallel-libs","perf-tools","provisioning","rms", "runtimes","serial-libs");
my %ohpcCategoryHeadings = ('admin' => 'Administrative Tools',
			    'compiler-families' => 'Compiler Families',
			    'dev-tools' => 'Development Tools',
			    'distro-packages' => 'Updates of Distro-provided packages',
			    'io-libs' => 'IO Libraries',
			    'lustre' => 'Lustre packages',
			    'mpi-families' => 'MPI Runtime/Transport Families',
			    'parallel-libs' => 'Parallel Libraries',
			    'perf-tools' => 'Performance Tools',
			    'provisioning' => 'Provisioning Tools',
			    'rms' => 'Resource Management',
			    'runtimes' => 'Runtimes',
			    'serial-libs' => 'Serial / Threaded Libraries');
			
			 
my @compiler_familes = ("gnu","gnu7","intel","gnu8","gnu9","gnu12","gnu13","gnu14","arm1");
my @mpi_families     = ("mvapich2","openmpi","openmpi3","openmpi4","impi","mpich");

my @package_skip = ("ohpc-release","gnu-compilers","R_base","mvapich2-psm","openmpi-psm2","scotch",
                    "openpbs-client","openpbs-execution","warewulf-cluster","warewulf-provision","warewulf-ipmi","warewulf-vnfs",
		     "warewulf-common-ohpc-localdb","lmod-defaults-gnu9-mpich-ofi","lmod-defaults-gnu9-mpich-ucx",
                     "python34-build-patch","python34-scipy","python34-numpy","python34-mpi4py","python3-Cython");

my %package_equiv = ("gnu7-compilers" => "Gnu Compiler Suite",
		     "gnu8-compilers" => "Gnu Compiler Suite",
		     "intel-compilers-devel" => "Intel Compiler Compatibility Package",
		     "llvm4-compilers" => "LLVM Compiler Suite",
		     "llvm5-compilers" => "LLVM Compiler Suite",
                     "intel-mpi-devel" => "Intel MPI Compatibility Package",
                     "openpbs-server" => "OpenPBS",
		     "openmpi3" => "openmpi",
		     "openmpi3-pmix-slurm" => "openmpi",
                     "warewulf-common" => "warewulf",
                     "wxparaver" => "paraver",
                     "ptscotch" => "scotch");
my @package_uniq_delim = ("slurm");

my $help;
my $category_single;
my $version;
my $addlpackages;
my $i;

GetOptions("h"          => \$help,
	   "version=s"  => \$version,
	   "package=s"  => \$addlpackages,
           "category=s" => \$category_single ) || usage();

if($help) {usage()};
if($category_single) {
    print "--> updating table contents for $category_single only\n";
    @ohpcCategories = $category_single;
}

if($version) {
    print "--> using version=$version for analysis\n";
} else {
    die("Please specify desired version to analyze with --version");
}


my @addons=();

if($addlpackages) {
    print "--> Included hard-coded components: $addlpackages\n";
    @addons=split(',',$addlpackages);
}

# version-specific packages to skip (ie. when new variants introduced)

if( versioncmp($version,"1.3.1") >= 0 ) {
    push(@package_skip,"gnu-compilers");
}



my $REMOVE_HTTP=0;
my $component_cnt=0;

my $header = << "END";
This page summarizes the packaged components available in the OpenHPC repository. These components are organized by groupings based on their general functionality, and links point to where additional information can be obtained for each component. Note that many of the 3rd party community libraries that are pre-packaged with OpenHPC are built using multiple compiler and MPI families. In these cases, the underlying RPM package names include delimiters identifying the development environment for which each package build is targeted.
END

print $header;

foreach my $category (@ohpcCategories) {
    print "\n### $ohpcCategoryHeadings{$category}:\n";


    my $filein="pkg-ohpc.$category";
    next unless (-s $filein);
    open(IN,"<$filein")  || die "Cannot open file -> $filein\n";

    
    # Read in data and cache

    my @nameData    = ();
    my @versionData = ();
    my @urlData     = ();
    my @summaryData = ();
    
    OUTER: while(<IN>) {

	# example format
	# pdsh-ohpc 2.31 https://github.com/chaos/pdsh ohpc/admin Parallel remote shell program
	if($_ =~ /^(\S+) (\S+) (\S+) (ohpc\/\S+) (.+)$/) {
	    my $name=$1;
	    my $version=$2;
	    my $url_raw=$3;

	    # strip http(s) from url

	    my $url = $url_raw;

	    if($REMOVE_HTTP) {
		if( $url_raw =~ /http:\/\/(\S+)/) {
		    $url=$1;
		} elsif ( $url_raw =~ /https:\/\/(\S+)/) {
		    $url=$1;
		}
	    }

	    # trim final slash from url

	    if ($url =~/(.*)\/$/) {
		$url = $1;
	    }

	    # package to skip?
	    foreach my $skip (@package_skip) {
		if ( $name =~ /^$skip/ ) {
		    next OUTER;
		}
	    }

	    # compiler/mpi package

	    foreach my $compiler (@compiler_familes) {
		foreach my $mpi (@mpi_families) {
		    if($name =~ /(\S+)-$compiler-ohpc/) {
			$name = $1;
		    } elsif ($name =~ /(\S+)-$compiler-$mpi-ohpc/) {
			$name = $1;
		    }
		}
	    }

	    # cull of -ohpc delimiter
	    if ( $name =~ /^(\S+)-ohpc$/ ) {
		$name = $1;
	    }

	    # non compiler/MPI package that should have unique identifier
	    foreach my $uniq (@package_uniq_delim) {
		if ( $name =~ /^$uniq-/ ) {
		    next OUTER;
		}
	    }

	    # hack for sionlib which changed Groups during 1.3.x series

	    if ( $name eq "sionlib" && $category eq "perf-tools" ) {
		next OUTER;
	    }

	    # name to replace?
	    if ( exists $package_equiv{$name} ) {
		$name = $package_equiv{$name};
	    }

	    # already known?
	    if(@nameData) {
		if ( $name eq $nameData[-1] ) {
		    next OUTER;
		}
	    }
	    
	    push(@nameData,   $name);
	    push(@versionData,$version);
	    push(@urlData,    $url);
	}
    }

    close(IN);

    # include any hard-coded additions
    if ($addlpackages) {
	foreach my $addon (@addons) {
	    my @entry = split('\|',$addon);
	    if ( $entry[0] eq $category ) {
		push(@nameData,$entry[1]);
		push(@versionData,'unknown');
		push(@urlData,$entry[2]);
	    }
	}
    }


    $i = 0;
    foreach my $name (@nameData) {
	print "- **`$name`** $urlData[$i]\n";
	$i++;
    }

    $component_cnt += scalar @nameData;

}

print "----------------------------\n";
print "# of Components = $component_cnt\n";

