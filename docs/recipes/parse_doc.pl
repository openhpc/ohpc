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
use File::Basename;
use File::Spec;

# Optional command-line arguments
my $repo = "";
my $validation_mode=0;

GetOptions ('repo=s' => \$repo,
	    "validation_mode" => \$validation_mode);

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

my $inputDir = dirname(File::Spec->rel2abs($file));
my $basename = basename($file,".tex");

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

# Determine BaseOS and define package manager commands

my $Install            = "";
my $chrootInstall      = "";
my $groupInstall       = "";
my $groupChrootInstall = "";

# parse package command macro's from input file

open(IN,"<$file")  || die "Cannot open file -> $file\n";
while(my $line = <IN>) {
    chomp($line);
    if($line =~ /\\newcommand{\\install}{(.+)}/ ) {
	$Install = $1;
    } 
    elsif ($line =~ /\\newcommand{\\chrootinstall}{(.+)}/ ) {
	$chrootInstall = $1;
    }
    elsif ($line =~ /\\newcommand{\\groupinstall}{(.+)}/ ) {
	$groupInstall = $1;
    }
    elsif ($line =~ /\\newcommand{\\groupchrootinstall}{(.+)}/ ) {
	$groupChrootInstall = $1;
    }
}

# Strip escape \ from latex macro

$chrootInstall      =~ s/\\\$/\$/;
$groupChrootInstall =~ s/\\\$/\$/;

# print "Install             = $Install\n";
# print "chrootInstall       = $chrootInstall\n";
# print "groupInstall        = $groupInstall\n";
# print "groupChrootInstall  = $groupChrootInstall\n";

if ($Install eq "" || $chrootInstall eq "" || $groupInstall eq "" || $groupChrootInstall eq "") {
    print "Error: package manager macros not defined\n";
    exit 1;
}

close(IN);

sub check_for_section_replacement {
    my $comment = shift;

    if ($comment =~ /\\ref{sec:(\S+)}/) {
	my $secname = $1;
	my $replacementText="";
	
	# We can only replace section information if latex document was built - otherwise we remove the section info
	if ( -e "$inputDir/$basename.aux" ) { 

	    my $secnum=`grep {sec:$secname} $inputDir/$basename.aux  | awk -v FS="{{|})" '{print \$2}' | awk -F '}' '{print \$1}'`;
	    
	    chomp($secnum);
	    
	    if ($secnum eq "") {
		die "Unable to query section number, verify latex build is up to date"
	    }
	    
	    $replacementText="(Section $secnum)";
	}

	$comment =~ s/\\ref{sec:(\S+)/$replacementText/g;
    }

    return($comment);
} # end check_for_section_replacement()

sub update_cmd {
    my $cmd = shift;

    $cmd =~ s/\(\*\\install\*\)/$Install/;
    $cmd =~ s/\(\*\\chrootinstall\*\)/$chrootInstall/;
    $cmd =~ s/\(\*\\groupinstall\*\)/$groupInstall/;
    $cmd =~ s/\(\*\\groupchrootinstall\*\)/$groupChrootInstall/;

    return($cmd);
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
	} elsif ($line =~ /^sms_eth_internal=(\S+)$/) {
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
}

# The input .tex file likely includes other input files via \input{foo}.  Do a
# first pass through the input file and accumulate a temp file which includes
# an aggregate copy of all the tex commands.

(my $fh_raw, my $tmpfile_raw ) = tempfile();

open(IN,"<$file")  || die "Cannot open file -> $file\n";

while(my $line = <IN>) {

    # Check for include 
    if( $line =~ /\\input{(\S+)}/ ) {

	my $inputFile = $1;
	# verify input file has .tex extension or add it

	if($inputFile !~ /(\S+).tex/ ) {
	    $inputFile = $inputFile . ".tex";
	}

	open(IN2,"<$inputFile") || die "Cannot open embedded input file $inputFile for parsing";

	while(my $line_embed = <IN2>) {
	    if( $line_embed =~ /\\input{\S+}/ ) {
		print "Error: nested \\input{} file parsing not supported\n";
		exit 1;
	    } elsif ($line !~ /^%/) {
		print $fh_raw $line_embed;
	    }
	}		
	close(IN2);
    } else {
	print $fh_raw $line;
    }
}

close(IN);
close($fh_raw);

# Next, parse the raw file and look for commands to execute based on delimiter

(my $fh,my $tmpfile) = tempfile();

open(IN,"<$tmpfile_raw")  || die "Cannot open file -> $file\n";

my $begin_delim   = "% begin_fsp_run";
my $end_delim     = "% end_fsp_run";
my $prompt        = "\[master\]\$";
my $disable_delim = "^%%";

print $fh "#!/bin/bash\n";

while(<IN>) {
    if( $_ =~ /$disable_delim/ ) {
	next;
    }
    if(/$begin_delim/../$end_delim/) {
	if ($_ =~ /% fsp_validation_newline/) {
	    print $fh "\n";
	} elsif ($_ =~ /% fsp_ci_comment (.+)/) {
	    if ( !$ci_run) {next;}
	    print $fh "# $1 (CI only)\n";
	} elsif ($_ =~ /% fsp_comment_header (.+)/) {

	    my $comment = check_for_section_replacement($1);
	    my $strlen  = length $comment;

	    printf $fh "\n";
	    printf $fh "# %s\n", '-' x $strlen;
	    print  $fh "# $comment\n";
	    printf $fh "# %s\n", '-' x $strlen;
	    
	} elsif ($_ =~ /% fsp_validation_comment (.+)/) {
	    my $comment = check_for_section_replacement($1);
	    print $fh "# $comment\n";
	} elsif ($_ =~ /% fsp_command (.+)/) {
	    my $cmd = update_cmd($1);
	    print $fh "$cmd\n";
	} elsif ($_ =~ /\[master\]\$ (.+) \\$/) {

	    my $cmd = update_cmd($1);

	    if($_ =~ /^%/ && !$ci_run ) { next; } # commands that begin with a % are for CI only

	    print $fh "$cmd";
	    my $next_line = <IN>;

	    # trim leading and trailing space
	    $next_line =~ s/^\s+|\s+$//g;

	    print $fh " $next_line\n";

	    # TODO - add loop to accomodate multi-line continuation
	} elsif ($_ =~ /\[master\]\$ (.+) #(.+)$/) {
	    my $cmd = update_cmd($1);

	    if($_ =~ /^%/ && !$ci_run ) { next; } # commands that begin with a % are for CI only

	    print $fh "$cmd\n";
	} elsif ($_ =~ /\[master\]\$ (.+)$/) {
	    my $cmd = update_cmd($1);

	    my $extra_comment = "";

	    if($_ =~ /^%/ && !$ci_run ) { next; } # commands that begin with a % are for CI only

	    print $fh "$cmd\n";
	} elsif ($_ =~ /\[postgres\]\$ (.+)$/) {
	    my $cmd = update_cmd($1);
	    print $fh "$cmd\n";
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
	$line =~ s/<sms_eth_internal>/$sms_eth_internal/g;
	$line =~ s/<eth_provision>/$eth_provision/g;
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
