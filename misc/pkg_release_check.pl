#!/usr/bin/env perl
use strict;

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

foreach my $distro (@distros) {
    my @updates = getPackageList($release_version, $distro);
    chomp @updates;

    foreach my $update (@updates) {
        my $update_release_number = getPackageRelease($update);
        my $package = getPackageName($update);
        my $base_package = getBasePackage($package, $release_version, $distro);
        my $base_release_number = getPackageRelease($base_package);

        if ($base_release_number > $update_release_number) {
            if (getPackageVersion($update) le getPackageVersion($base_package)) {
                print "Error: $distro $update release number too low\n";
            }
        }
    }
}

sub getPackageList {
    my ($release, $distro) = @_;
    my ($major_ver, $minor_ver, $micro_ver) = split(/\./, $release);

    my $repo_id = "$release - Update #$micro_ver rolling development builds ($distro)";
    my $repo_url = "http://build.openhpc.community/OpenHPC:/$major_ver.$minor_ver:/Update$micro_ver:/Factory/$distro";
    my $query_args = "--repofrompath=\"$repo_id,$repo_url\" --repoid=\"$repo_id\" '*'";
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

