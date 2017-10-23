#!/usr/bin/env perl

use warnings;
use strict;

my @compiler_familes = ("gnu7");
my @mpi_families     = ("mpich", "mvapich2","openmpi3");

my $delim="ohpc";
my $merge_package_families = 0;

my @single_package_exceptions = ("");

sub parse_changes {
    my $infile     = shift;
    my $logMessage = shift;
    my %pkg_hash   = ();
    my %pkg_length = ();

    open(IN,"<$infile")  || die "Cannot open file -> $infile\n";

    while(<IN>) {
        my $name = "";
        my $old_version = "";
        my $version = "";

        if($_ =~ /^(\S+) (\S+) (\S+)$/) {
            $name        = $1;
            $old_version = $2;
            $version     = $3;
        } elsif ($_ =~ /^(\S+) (\S+)$/) {
            $name    = $1;
            $version = $2;
        } else {
            die("Unknown format in raw changlog files");
        }

        # Check if this is a compiler/MPI family package

        my $compiler_package = 0;
        my $mpi_package      = 0;
        my $name_base        = $1;
        
        foreach my $compiler (@compiler_familes) {
            foreach my $mpi (@mpi_families) {
                if($name =~ /(\S+)-$compiler-$delim/) {
                    die "unknown package family for compiler" unless ($mpi_package == 0);
                    $compiler_package = 1;
                    $name_base = $1;
                } elsif ($name =~ /(\S+)-$compiler-$mpi-$delim/) {
                    die "unknown package family for MPI" unless ($compiler_package == 0);
                    $mpi_package = 1;
                    $name_base = $1;
                }
            }
        }
        
	if($old_version ne "") {
	    printf OUT "      * %-40s %s\n",$name,"(v$old_version -> v$version)";
	} else {
	    printf OUT "      * %-40s %s\n",$name,"(v$version)";
	}
    }

    close(IN);
} # end parse_changes()



my $fileout="ChangeLog";
open(OUT,">$fileout")  || die "Cannot open file -> $fileout\n";

#parse_changes("pkg-ohpc.chglog-add","        * [NEW] component added   - ");
#parse_changes("pkg-ohpc.chglog-upd","        * [UPD] component updated - ");
#parse_changes("pkg-ohpc.chglog-del","        * [DEL] component removed - ");

### parse_changes("pkg-ohpc.chglog-add","* [NEW] component added   - ");
### parse_changes("pkg-ohpc.chglog-upd","* [UPD] component updated - ");
### parse_changes("pkg-ohpc.chglog-del","* [DEL] component removed - ");
### 
### print OUT "   [Component Changes]\n";
### parse_changes("pkg-ohpc.chglog-add","      * added   - ");
### parse_changes("pkg-ohpc.chglog-upd","      * updated - ");
### parse_changes("pkg-ohpc.chglog-del","      * removed - ");

print OUT "   [Component Additions]\n";
parse_changes("pkg-ohpc.chglog-add","      * ");
print OUT "\n   [Component Version Changes]\n";
parse_changes("pkg-ohpc.chglog-upd","      * ");
print OUT "\n   [Components Deprecated]\n";
parse_changes("pkg-ohpc.chglog-del","      * ");

close(OUT);






