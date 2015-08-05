#!/usr/bin/env perl
# 
# Simple utility to derive cluster-specific inputs for use with FSP 
# installation recipe. Typically used within CI environment.
# 
# karl.w.schulz@intel.com
#------------------------------------------------------------------------

use warnings;
#use strict;
use Getopt::Long;

# Optional command-line arguments
my $outputFile = "/tmp/input.local";
my $inputFile  = "/opt/fsp/pub/doc/recipes/vanilla/input.local/template";

sub update_var {
    my $varname = shift;
    my $value   = shift;

    if($_[0] =~ /^($varname=\"\${$varname:-)(\S+)}\"$/) {
	$_[0] = "$1"."$value}\"\n";
	return(1);
    } else {
	return(0);
    }
}

GetOptions ('o=s' => \$outputFile,
	    'i=s' => \$inputFile);

if ( $#ARGV < 0) {
    print STDERR "Usage: gen_inputs [OPTIONS] <hardware_mapfile>\n";
    print STDERR "\nwhere available OPTIONS are:\n";
    print STDERR "   -i input               Location of template input file (default=/opt/fsp/pub/doc/recipes/vanilla)\n";
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

my $Repo;
if( !defined $ENV{Repo} ) {
    print STDERR "Repo environment variable must be set to desired repo location\n";
    exit 1;
} else {
    $Repo=$ENV{Repo};
}

    
    

my $mapfile      = shift;
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

# Now, copy input -> output and update vars based on detected CI settings

open(IN,"<$inputFile")   || die ("Cannot open input  file -> $inputFile");
open(OUT,">$outputFile") || die ("Cannot open output file -> $outputFile");

# http://tcgsw-obs.pdx.intel.com:82/ForestPeak:/15.16/CentOS-7.1_Intel/ForestPeak:15.16.repo
# http://tcgsw-obs.pdx.intel.com:82/ForestPeak:/15.31:/Factory/CentOS-7.1_Intel/ForestPeak:15.31:Factory.repo

while(my $line=<IN>) {

    if( $line =~ m!^(fsp_repo=\"\${fsp_repo:-http://\S+)/(\S+)/(\S+)/(\S+).repo}\"$! ) {
 	if($Repo ne "Release" ) {
 	    print OUT "$1/$2:/$Repo/$3/$4:$Repo.repo}\"\n";
 	} else {
 	    print OUT $line;
 	}
    } elsif( update_var("master_name",$ENV{'NODE_NAME'},$line) ) {
	print OUT $line;
    } elsif ( update_var("master_ip",$master_ip,$line) ) {
	print OUT $line;
    } elsif ( update_var("sms_eth_internal",$sms_eth_internal,$line) ) {
	print OUT $line;
    } elsif ( update_var("internal_netmask",$netmask,$line) ) {
	print OUT $line;
    } elsif ( update_var("eth_provision",$eth_provision,$line) ) {
	print OUT $line;
    } elsif ( update_var("bmc_username",$bmc_username,$line) ) {
	print OUT $line;
    } elsif ( update_var("num_computes",$num_computes,$line) ) {
	print OUT $line;
    } elsif ( update_var("sms_ipoib",$master_ipoib,$line) ) {
	print OUT $line;
    } elsif ( update_var("mgs_fs_name",$mgs_fs_name,$line) ) {
	print OUT $line;
    } elsif ($line =~ /^c_ip\[(\d)\]=\S+/) {
 	print OUT "c_ip[$1]=$compute_ips[$1]\n";
    } elsif ($line =~ /^c_mac\[(\d)\]=\S+/) {
 	print OUT "c_mac[$1]=$compute_macs[$1]\n";
    } elsif ($line =~ /^c_bmc\[(\d)\]=\S+/) {
 	print OUT "c_bmc[$1]=$compute_bmcs[$1]\n";
    } elsif ($line =~ /^c_ipib\[(\d)\]=\S+/) {
 	print OUT "c_ipib[$1]=$compute_ipoibs[$1]\n";
    } else {
 	print OUT $line;
    }
}

close OUT;
print "Localized cluster input saved to $outputFile\n";
