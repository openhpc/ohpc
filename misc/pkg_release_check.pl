#!/usr/bin/env perl
use strict;
use warnings;
use Sort::Versions;

# basic usage
sub usage {
    print "\n";
    print "Usage: $0 RELEASE_VERSION\n\n";
    print "\n";
    exit 0;
}

my $release_version = shift@ARGV;
if (! $release_version) {
    usage;
}

my @distros = ('CentOS_7', 'SLE_12');
my @arches  = ('aarch64','x86_64', 'noarch');

my @problems = ();

foreach my $distro (@distros) {
    foreach my $arch (@arches) {
    my @updates = getPackageList($release_version, $distro, $arch);
    chomp @updates;

    print '-' x 50;
    print "\nAnalyzing release -> $release_version ($distro/$arch)\n";
    my %base_versions = cacheRepoPackages($release_version, $distro,$arch);

    foreach my $update (@updates) {
	my ($package,$update_version,$update_release) = split ' ',$update;

	if (! exists ($base_versions{$package}) ) {
	    print "[skipping new package $package]\n";
	    next;
	}

	my $base_version = $base_versions{$package}[0];
	my $base_release = $base_versions{$package}[1];

## # 	# for testing
## 	if($package eq "vim-clustershell-ohpc") {
## 	    $update_release="24.1";
##  	}


	# Sort the version strings...
	my @versions = ($base_version,$update_version);
	my @verSort = sort { versioncmp($a,$b) } @versions;

	# Check if we have an older package string (version+release) being published...
	if (versioncmp($base_release,$update_release) == 1 ) {
	    if ( $update_version eq $verSort[0] ) {
		print "--> $package: \n";
		print "    version (old,new) = $base_version,$update_version\n";
		print "    release (old,new) = $base_release,$update_release\n";

                print "\n    Error: $distro $update release number too low\n\n";
		push(@problems,$package);
	    }
	}
    }

    if (@problems) {
	print "\n";
	foreach my $problem (@problems) {
	    print "problem detected for package -> $problem\n";
	}
	exit(1);
    } else {
	print "\n All groovy for $distro/$arch\n\n";
    }
    }
}

sub getPackageList {
    my ($release, $distro, $arch) = @_;
    my ($major_ver, $minor_ver, $micro_ver) = split(/\./, $release);

    my $repo_id = "$release - Update #$micro_ver rolling development builds ($distro)";
    my $repo_url = "http://build.openhpc.community/OpenHPC:/$major_ver.$minor_ver:/Update$micro_ver:/Factory/$distro";
    my $query_args = "--repofrompath=\"$repo_id,$repo_url\" --repoid=\"$repo_id\" --queryformat='%{Name} %{Version} %{Release}' --archlist=$arch,noarch '*'";
    my @packages = `repoquery $query_args`;

    return @packages;
}

sub getPackageName {
    my $package = shift;
    my ($name, $ver) = split(/:/, $package);

    return substr $name, 0, -2;
}

sub getPackageRelease {
    my $package = shift;

    my ($name, $ver) = split(/:/, $package);
    my ($version, $rel) = split(/-/, $ver);
    my ($release, $extraver, $arch) = split(/\./, $rel);

    return $release;
}

sub getPackageVersion {
    my $package = shift;

    my ($name, $ver) = split(/:/, $package);
    my ($version, $rel) = split(/-/, $ver);

    return $version;
}
sub getBasePackage {
    my ($package, $release, $distro) = @_;

    my ($major_ver, $minor_ver, $micro_ver) = split(/\./, $release);
    my $base_repo_id = "$major_ver.$minor_ver - Base ($distro)";
    my $base_repo_url = "http://build.openhpc.community/OpenHPC:/$major_ver.$minor_ver/$distro";
    my $updates_repo_id = "$major_ver.$minor_ver - Updates ($distro)";
    my $updates_repo_url = "http://build.openhpc.community/OpenHPC:/$major_ver.$minor_ver/updates/$distro";
    my $query_args = "--repofrompath=\"$base_repo_id,$base_repo_url\" --repoid=\"$base_repo_id\" --repofrompath=\"$updates_repo_id,$updates_repo_url\" --repoid=\"$updates_repo_id\" '$package'";
    my $base_package = `repoquery $query_args`;

    return $base_package;
}


sub cacheRepoPackages {
    my ($release, $distro, $arch) = @_;
    my ($major_ver, $minor_ver, $micro_ver) = split(/\./, $release);
    
    my $base_repo_id = "$major_ver.$minor_ver - Base ($distro)";
    my $base_repo_url = "http://build.openhpc.community/OpenHPC:/$major_ver.$minor_ver/$distro";
    my $updates_repo_id = "$major_ver.$minor_ver - Updates ($distro)";
    my $updates_repo_url = "http://build.openhpc.community/OpenHPC:/$major_ver.$minor_ver/updates/$distro";
    my $query_args = "--repofrompath=\"$base_repo_id,$base_repo_url\" --repoid=\"$base_repo_id\" --repofrompath=\"$updates_repo_id,$updates_repo_url\" --repoid=\"$updates_repo_id\" --queryformat='%{Name} %{Version} %{Release}' --archlist=$arch,noarch '*'";

    my $base_package = `repoquery $query_args`;
    my @results = `repoquery $query_args`;
    chomp(@results);

    my %base_versions = ();

    foreach my $line (@results) {
	my ($name,$version,$release) = split ' ',$line;
	push(@{$base_versions{$name}},$version);
	push(@{$base_versions{$name}},$release);
    }

    my $verCount = keys %base_versions;

    print "\nNumber of packages from base config = $verCount\n\n";


    return %base_versions;
}

