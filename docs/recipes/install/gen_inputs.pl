#!/usr/bin/env perl
# 
# Simple utility to derive cluster-specific inputs for use with FSP 
# installation recipe. Typically used within CI environment.
# 
# karl.w.schulz@intel.com
#------------------------------------------------------------------------

use warnings;
use strict;
use Getopt::Long;

# Optional command-line arguments
my $outputFile = "/tmp/input.local";

GetOptions ('o=s' => \$outputFile);

if ( $#ARGV < 0) {
    print STDERR "Usage: gen_inputs [OPTIONS] <hardware_mapfile>\n";
    print STDERR "\nwhere available OPTIONS are:\n";
    print STDERR "   -o output              Location of output file (default=/tmp/input.local)\n";
    print STDERR "\n";
    exit 1;
}

my $BaseOS;
if( !defined $ENV{BaseOS} ) {
    print STDERR "BaseOS environment variable must be set to choice of OS\n";
    exit 1;
} else {
    $BaseOS=$ENV{BaseOS};
}

my $mapfile      = shift;
my $master_host  = "";
my @computes     = ();
my $num_computes = 0;

open(OUT,">$outputFile") || die ("Cannot create output file -> $outputFile");

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
} else {
    die "Unknown CI environment";
}

# Obtain dynamic host info

my $nfs_ip           = "";
my $master_ip        = "";
my $master_ipoib     = "";
my $netmask          = "";
my $ipoib_netmask    = "";
my $mgs_fs_name      = "";
my $bmc_username     = "";
my $bmc_password     = "";
my $sms_eth_internal = "";
my $eth_provision    = "";
my @compute_ips      = ();
my @compute_ipoibs   = ();
my @compute_macs     = ();
my @compute_bmcs     = ();

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
    } elsif ($line =~ /^sms_eth_internal_$BaseOS=(\S+)$/) {
	$sms_eth_internal = $1;
    } elsif ($line =~ /^eth_provision=(\S+)$/) {
	$eth_provision = $1;
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

die "Unable to map compute IPs"  if (! @compute_ips);
die "Unable to map compute MACs" if (! @compute_macs);
die "Unable to map compute BMCs" if (! @compute_bmcs);

print OUT "master_hostname=$ENV{'NODE_NAME'}\n";
print OUT "master_ip=$master_ip\n";
print OUT "sms_eth_internal=$sms_eth_internal\n";
print OUT "ipoib_netmask=$ipoib_netmask\n";
print OUT "sms_ipoib=$master_ipoib\n";
print OUT "internal_netmask=$netmask\n";
print OUT "eth_provision=$eth_provision\n";
print OUT "mgs_fs_name=$mgs_fs_name\n";

for (my $i=1;$i<=$num_computes; $i++) {
    printf OUT "c%i_ip=%s\n",$i,$compute_ips[$i-1];
}
 for (my $i=1;$i<=$num_computes; $i++) {
    printf OUT "c%i_mac=%s\n",$i,$compute_macs[$i-1];
 }
for (my $i=1;$i<=$num_computes; $i++) {
    printf OUT "c%i_bmc=%s\n",$i,$compute_bmcs[$i-1];
}
 for (my $i=1;$i<=$num_computes; $i++) {
    printf OUT "c%i_ipoib=%s\n",$i,$compute_ipoibs[$i-1];
 }

close OUT;
print "Localized cluster input saved to $outputFile\n";
