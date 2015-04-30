#!/usr/bin/env perl
# 
# Simple parser to cull command-line instructions from documentation for
# validation.
# 
# karl.w.schulz@intel.com
#------------------------------------------------------------------------

use warnings;
use strict;
use File::Temp qw/tempfile/;
use Getopt::Long;

# Optional command-line arguments
my $repo = "";

GetOptions ('repo=s' => \$repo);

if ( $#ARGV < 1) {
    print STDERR "Usage: parse_doc <filename> <host_mapping>\n";
    exit 1;
}

my $file         = shift;
my $mapfile      = shift;
my $ci_run       = 0;
my $master_host  = "";
my @computes     = ();
my $num_computes = 0;

# Test for CI environment

if ( defined $ENV{'NODE_NAME'} && defined $ENV{'COMPUTE_HOSTS'} ) {
    $master_host = $ENV{'NODE_NAME'};

    if ($master_host =~ /(master[1-9])-(\S+)/ ) {
	$master_host = $1;
    } elsif ( $master_host =~ /(sms[1-9])/ ) {
	$master_host = $1;
    }
    @computes = split(', ',$ENV{'COMPUTE_HOSTS'});
    $num_computes = @computes;
    $ci_run = 1;
} 

# Obtain dynamic host info

my $nfs_ip         = "";
my $master_ip      = "";
my $master_ipoib   = "";
my $netmask        = "";
my $ipoib_netmask  = "";
my $mgs_fs_name    = "";
my $bmc_username   = "";
my $bmc_password   = "";
my @compute_ips    = ();
my @compute_ipoibs = ();
my @compute_macs   = ();
my @compute_bmcs   = ();

if($ci_run == 1) {

    open(IN,"<$mapfile")  || die "Cannot open file -> $mapfile\n";

    while(my $line=<IN>) {
	if($line =~ /^$master_host\_ip=(\S+)$/) {
	    $master_ip = $1;
	} elsif ($line =~ /^$master_host\_ipoib=(\S+)$/) {
	    $master_ipoib = $1;
	} elsif ($line =~ /^nfs_ip=(\S+)$/) {
	    $nfs_ip = $1;
	} elsif ($line =~ /^mgs_fs_name=(\S+)$/) {
	    $mgs_fs_name = $1;
	} elsif ($line =~ /^$master_host\_netmask=(\S+)$/) {
	    $netmask = $1;
	} elsif ($line =~ /^ipoib_netmask=(\S+)$/) {
	    $ipoib_netmask = $1;
	} elsif ($line =~ /^bmc_username=(\S+)$/) {
	    $bmc_username = $1;
	} elsif ($line =~ /^bmc_password=(\S+)$/) {
	    $bmc_password = $1;
	} else {
	    foreach my $compute (@computes) {
		if ($line =~ /^$compute\_ip=(\S+)$/) {
		    push(@compute_ips,$1);
		}
		if ($line =~ /^$compute\_ipoib=(\S+)$/) {
		    push(@compute_ipoibs,$1);
		}
		if ($line =~ /^$compute\_mac=(\S+)$/) {
		    push(@compute_macs,$1);
		}
		if ($line =~ /^$compute\_bmc=(\S+)$/) {
		    push(@compute_bmcs,$1);
		}
	    }
	}
    }

    close(IN);
}

(my $fh,my $tmpfile) = tempfile();

open(IN,"<$file")  || die "Cannot open file -> $file\n";

my $begin_delim = "% begin_fsp_run";
my $end_delim   = "% end_fsp_run";
my $prompt      = "\[master\]\$";

print $fh "#!/bin/bash\n";

print $fh "NUM_COMPUTES=$num_computes\n";

while(<IN>) {
    if(/$begin_delim/../$end_delim/) {
	if ($_ =~ /% fsp_validation_comment (.+)/) {
	    print $fh "echo \"-------------------------------------------------------------------\"\n";
	    print $fh "echo \"$1\"\n";
	    print $fh "echo \"-------------------------------------------------------------------\"\n";
	} elsif ($_ =~ /\[master\]\$ (.+) \\$/) {
	    print $fh "$1";
	    my $next_line = <IN>;

	    #           trim leading and trailing space
	    $next_line =~ s/^\s+|\s+$//g;

	    print $fh " $next_line\n";

	    # TODO - add loop to accomodate multi-line continuation
	} elsif ($_ =~ /\[master\]\$ (.+) #(.+)$/) {
	    print $fh "$1\n";
	} elsif ($_ =~ /\[master\]\$ (.+)$/) {
	    print $fh "$1\n";
	} elsif ($_ =~ /\[postgres\]\$ (.+)$/) {
	    print $fh "$1\n";
	}

    }
}

close(IN);
close($fh);

# Replace variables if running in CI environment

open(IN,"<$tmpfile")  || die "Cannot open file -> $tmpfile\n";

while(my $line=<IN>) {
    if($ci_run == 1) {
	$line =~ s/<nfs_ip>/$nfs_ip/g;
	$line =~ s/<master_ip>/$master_ip/g;
	$line =~ s/<internal_netmask>/$netmask/g;
	$line =~ s/<master_ipoib>/$master_ipoib/g;
	$line =~ s/<ipoib_netmask>/$ipoib_netmask/g;
	$line =~ s/<bmc_username>/$bmc_username/g;
	$line =~ s/<bmc_password>/$bmc_password/g;
	$line =~ s/<mgs_fs_name>/$mgs_fs_name/g;
	#	$line =~ s/<master_hostname>/$master_host/g;
	$line =~ s/<master_hostname>/$ENV{'NODE_NAME'}/g;

	# Support for optionally defined FSP repo

	if ( $repo ne "" ) {
	    $line =~ s/zypper addrepo (\S+)/zypper addrepo $repo/g;                     # SLES
            $line =~ s|wget -P /etc/yum.repos.d (\S+)|wget -P /etc/yum.repos.d $repo|g; # CentOS
	}

	for (my $i=1;$i<=$num_computes; $i++) {
	    $line =~ s/<c$i\_ip>/$compute_ips[$i-1]/g;
	    $line =~ s/<c$i\_ipoib>/$compute_ipoibs[$i-1]/g;
	    $line =~ s/<c$i\_mac>/$compute_macs[$i-1]/g;
	    $line =~ s/<c$i\_bmc>/$compute_bmcs[$i-1]/g;
	}

	# addition for dynamic sizing - if any compute hosts remain - remove their <> brackets

	$line =~ s/<c(\d+)\_ip>/c$1\_ip/g;
	$line =~ s/<c(\d+)\_ipoib>/c$1\_ipoib/g;
	$line =~ s/<c(\d+)\_mac>/c$1\_mac/g;
	$line =~ s/<c(\d+)\_bmc>/c$1\_bmc/g;

	print $line;

    } else {
	print $line;
    }
}

unlink($tmpfile) || die("Unable to remove $tmpfile");
