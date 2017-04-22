#!/usr/bin/env perl
# --------------------------------------------------------------------
# Simple support script for header management.  Replaces header text
# found within delimiter with provided text.
# --------------------------------------------------------------------

use warnings;
use File::Compare;
use File::Basename;
use Getopt::Long;

my $license_begin_delim='----------bh-$';
my $license_end_delim='----------eh-$';

# Command-line parsing

GetOptions("S=s@","c2f_comment","c2sh_comment") || die "Error using GetOptions";

if (@ARGV >= 2) {
    $header_file = shift @ARGV;
} else {
    print "\nUsage: update_license.pl HEADER-FILE SOURCE-FILES...\n\n";
    exit 0;
}

# Verify license file
# existence and cache contents.

if ( ! -s $header_file ) {
    print "\n** Error: unable to open valid license file ($header_file).\n\n";
    exit(1);
}

open ($HEAD,$header_file) || die "Cannot open $header_file\n";
my @license_text = <$HEAD>;
close($HEAD);

# Scan all provided input files and look for designated header
# begin/end delimiters.  When found, update with provided header file
# contents.

my $found_delim = 0;

while (@ARGV)
{
    $found_delim = 0;

    $infile_test = shift @ARGV;

    # autoconf support - look for ".in" version of the src file

    if ( -e "$infile_test.in" ) {
        $infile = "$infile_test.in";
    } else {
        $infile = "$infile_test";
    }

    open($IN, "<$infile") || die "Cannot open $infile\n";

    my $basename = basename("$infile");
    my $dirname  = dirname("$infile");
    my $tmpfile  = "$dirname/.$basename.tmp";

    # Let's punt if we cannot create tmp files locally

    if ( ! open ($TMPFILE,">$tmpfile") ) {
        print "[header_tool]: Warning -> unable to create tmp file locally ($tmpfile) - aborting update.\n";
        next;
    }

    while (<$IN>) {
        if (/$license_begin_delim/../$license_end_delim/) {
            $found_delim=1;
            if (/$license_begin_delim/) {
                print $TMPFILE @license_text;
            }
        } else {
            print $TMPFILE $_;
        }
    }

    close($IN);
    close($TMPFILE);

    if ( $found_delim ) {
        if ( compare($infile,$tmpfile) != 0 )  {
            print "[header_tool]: updating license in file $infile\n";

            # cache perms of original file so we can mirror them
            my $mode_orig = (stat($infile))[2] & 0777;

            rename($tmpfile,$infile) || die "Cannot rename updated file\n";
            chmod($mode_orig,$infile) || die "Cannot chmod permissions to match original\n";
        } else {
            unlink($tmpfile) || die "Unable to remove temporary file\n";
        }
    } else {
        unlink($tmpfile) || die "Unable to remove temporary file\n";
    }
}
