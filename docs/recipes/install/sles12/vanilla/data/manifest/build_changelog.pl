#!/usr/bin/env perl

use warnings;
use strict;

my @compiler_familes = ("gnu","intel");
my @mpi_families     = ("mvapich2","openmpi","impi");

my $delim="fsp";

my @single_package_exceptions = ("lmod-defaults-intel-fsp");

sub parse_changes {
    my $infile     = shift;
    my $logMessage = shift;
    my %pkg_hash = ();

    open(IN,"<$infile")  || die "Cannot open file -> $infile\n";

    while(<IN>) {
        my $name = "";
        my $version = "";

        if($_ =~ /^(\S+) (\S+) (\S+)$/) {
            $name    = $1;
            $version = $3;
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
        
        # hash the compiler/mpi packages to verify we detect the same
        # version for all combinations
        
        if($compiler_package || $mpi_package) {
            if (! defined $pkg_hash{$name_base} ) {
                $pkg_hash{$name_base} = $version;
                print OUT $logMessage . "$name_base-*-fsp (v$version)\n";
            } else {
                if($version ne $pkg_hash{$name_base} ) {
                    print "ERROR: versions inconsistent for $name_base family\n";
                    exit(1);
                }
            }
        } else {
            print OUT $logMessage . "$name (v$version)\n";
        }
    }

    close(IN);
} # end parse_changes()



my $fileout="ChangeLog";
open(OUT,">$fileout")  || die "Cannot open file -> $fileout\n";

#parse_changes("pkg-fsp.chglog-add","        * [NEW] component added   - ");
#parse_changes("pkg-fsp.chglog-upd","        * [UPD] component updated - ");
#parse_changes("pkg-fsp.chglog-del","        * [DEL] component removed - ");

parse_changes("pkg-fsp.chglog-add","* [NEW] component added   - ");
parse_changes("pkg-fsp.chglog-upd","* [UPD] component updated - ");
parse_changes("pkg-fsp.chglog-del","* [DEL] component removed - ");

close(OUT);






