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
my $repo   = "";
my $ci_run = 0;

GetOptions ('repo=s' => \$repo,
	    'ci_run' => \$ci_run);

if ( $#ARGV < 0 ) {
print "$repo $ci_run\n";
    print STDERR "Usage: parse_doc [--repo=<reponame>] [--ci_run] <filename>\n";
    print STDERR "  --repo	Use a different repo for install commands\n";
    print STDERR "  --ci_run	run additional 'CI only' commands\n";
    exit 1;
}

my $file        = shift;
my $inputDir    = dirname(File::Spec->rel2abs($file));
my $basename    = basename($file,".tex");
chdir $inputDir;

# Determine BaseOS and define package manager commands
my $Install            = "";
my $chrootInstall      = "";
my $groupInstall       = "";
my $groupChrootInstall = "";

# parse package command macro's from input file
open( IN, "<$basename.tex" ) || die __LINE__ . ": Cannot open file -> $file\n$!";
while( my $line = <IN> ) {
    chomp( $line );
    if( $line =~ /\\newcommand\{\\install\}\{(.+)\}/ ) {
	$Install = $1;
    } 
    elsif( $line =~ /\\newcommand\{\\chrootinstall\}\{(.+)\}/ ) {
	$chrootInstall = $1;
    }
    elsif( $line =~ /\\newcommand\{\\groupinstall\}\{(.+)\}/ ) {
	$groupInstall = $1;
    }
    elsif( $line =~ /\\newcommand\{\\groupchrootinstall\}\{(.+)\}/ ) {
	$groupChrootInstall = $1;
    }
}
close( IN );

# Strip escape \ from latex macro
$chrootInstall      =~ s/\\\$/\$/;
$groupChrootInstall =~ s/\\\$/\$/;

# print "Install             = $Install\n";
# print "chrootInstall       = $chrootInstall\n";
# print "groupInstall        = $groupInstall\n";
# print "groupChrootInstall  = $groupChrootInstall\n";

if( $Install eq "" || $chrootInstall eq "" || $groupInstall eq "" || $groupChrootInstall eq "" ) {
    print "Error: package manager macros not defined\n";
    exit 1;
}

# The input .tex file likely includes other input files via \input{foo}.  Do a
# first pass through the input file and accumulate a temp file which includes
# an aggregate copy of all the tex commands.

( my $TMPFH_AGGR_STEPS, my $tmpfile_aggr_steps ) = tempfile();
open( IN, "<$basename.tex" ) || die __LINE__ . ": Cannot open file -> $file\n$!";

while( my $line = <IN> ) {
    # Check for include 
    if( $line =~ /\\input\{(\S+)\}/ ) {
	my $inputFile = $1;

	# verify input file has .tex extension or add it
	if( $inputFile !~ /(\S+).tex/ ) {
	    $inputFile = $inputFile . ".tex";
	}

	open( IN2,"<$inputFile" ) || die __LINE__ . ": Cannot open embedded input file $inputFile for parsing\n$!";

	while( my $line_embed = <IN2> ) {
	    if( $line_embed =~ /\\input\{\S+\}/ ) {
		print "Error: nested \\input{} file parsing not supported\n";
		exit 1;
	    } elsif( $line !~ /^%/ ) {
		print $TMPFH_AGGR_STEPS $line_embed;
	    }
	}		
	close( IN2 );
    } else {
	print $TMPFH_AGGR_STEPS $line;
    }
}

close( IN );
close( $TMPFH_AGGR_STEPS );

# Next, parse the raw file and look for commands to execute based on delimiter
( my $fh,my $tmpfile ) = tempfile();

open( IN, "<$tmpfile_aggr_steps" ) || die __LINE__ . ": Cannot open file -> $file\n$!";

my $begin_delim   = "% begin_ohpc_run";
my $end_delim     = "% end_ohpc_run";
my $prompt        = "\[master\]\(\*\\\#\*\)";
my $disable_delim = "^%%";
my $indent        = 0;

print $fh "#!/bin/bash\n";

while( <IN> ) {
    if( $_ =~ /$disable_delim/ ) {
	next;
    }
    if( /$begin_delim/../$end_delim/ ) {
	if( $_ =~ /% ohpc_validation_newline/ ) {
	    print $fh "\n";
	} elsif( $_ =~ /% ohpc_ci_comment (.+)/ ) {
	    if ( !$ci_run ) { next; }
	    print $fh "# $1 (CI only)\n";
	} elsif ( $_ =~ /% ohpc_comment_header (.+)/ ) {
	    my $comment = check_for_section_replacement( $1 );
	    my $strlen  = length $comment;

	    printf $fh "\n";
	    printf $fh "# %s\n", '-' x $strlen;
	    print  $fh "# $comment\n";
	    printf $fh "# %s\n", '-' x $strlen;
	} elsif( $_ =~ /% ohpc_validation_comment (.+)/ ) {
	    my $comment = check_for_section_replacement( $1 );
	    print $fh ' ' x $indent . "# $comment\n";
	} elsif( $_ =~ /% ohpc_indent (\d+)/ ) {
	    $indent = $1;
	} elsif($_ =~ /% ohpc_command (.+)/ ) {
	    my $cmd = update_cmd( $1 );
	    print $fh ' ' x $indent . "$cmd\n";
	} elsif( $_ =~ /\[master\]\(\*\\\#\*\) (.+ <<([^ ]+).*)$/ ) {
	    my $cmd  = update_cmd($1);
	    my $here = $2;

	    if( $_ =~ /^%/ && !$ci_run ) { next; } # commands that begin with a % are for CI only

            print $fh ' ' x $indent . "$cmd\n";
	    my $next_line;
	    do {
	        $next_line = <IN>;
	        # trim leading and trailing space
	        $next_line =~ s/^\s+|\s+$//g;
	      
	        print $fh "$next_line\n";
	     } while( $next_line !~ /^$here/ );

	} elsif( $_ =~ /\[master\]\(\*\\\#\*\) (.+) \\$/ ) {
	    my $cmd = update_cmd( $1 );

	    if( $_ =~ /^%/ && !$ci_run ) { next; } # commands that begin with a % are for CI only

	    print $fh ' ' x $indent . "$cmd";
	    my $next_line = <IN>;

	    # trim leading and trailing space
	    $next_line =~ s/^\s+|\s+$//g;

	    print $fh " $next_line\n";

	    # TODO - add loop to accomodate multi-line continuation
	} elsif ($_ =~ /\[master\]\(\*\\\#\*\) (.+[ ]*;[ ]*do)$/) {
	    # special treatment for do loops
	    my $cmd = update_cmd($1);
	    
	    print $fh ' ' x $indent . "$cmd\n";
	    my $next_line;

	    while ( $next_line = <IN> ) {
		last if $next_line =~ m/\s+done/;

		# trim leading and trailing space
		$next_line =~ s/^\s+|\s+$//g;

		printf $fh ' ' x $indent . "   %s\n",$next_line;
	    }

	    # trim leading and trailing space
	    $next_line =~ s/^\s+|\s+$//g;

	    print $fh ' ' x $indent . "$next_line\n";
	} elsif( $_ =~ /\[master\]\(\*\\\#\*\) (.+)$/ ) {
	    my $cmd = update_cmd($1);

	    if( $_ =~ /^%/ && !$ci_run ) { next; } # commands that begin with a % are for CI only

	    print $fh ' ' x $indent . "$cmd\n";
	} elsif( $_ =~ /\[postgres\]\$ (.+)$/ ) {
	    my $cmd = update_cmd( $1 );
	    print $fh ' ' x $indent . "$cmd\n";
	}

    }
}

close( IN );
close( $fh );

# Echo commands
open( IN, "<$tmpfile" ) || die __LINE__ . ": Cannot open file -> $tmpfile\n$!";
while ( <IN> ) {
    print;
}

unlink( $tmpfile ) || die "Unable to remove $tmpfile\n$!";




sub check_for_section_replacement {
    my $comment = shift;

    if( $comment =~ /\\ref\{sec:(\S+)\}/ ) {
	my $secname = $1;
	my $replacementText = "";
	
	# We can only replace section information if latex document was built - otherwise we remove the section info
	if( -e "$inputDir/$basename.aux" ) { 
            open( FH, "$inputDir/$basename.aux" ) || die __LINE__ . ": Cannot open file -> $inputDir/$basename.aux\n$!";
	    my $secnum;
            while( <FH> ) {
                if( /\{sec:$secname\}\{\{([^\}]+)/ ) {
                    if( defined $secnum ) { die __LINE__ . ": found duplicate section name $secname ($secnum vs $1)"; }
                    $secnum = $1;
                }
            }
            close( FH );
            chomp( $secnum );
	    
	    if( $secnum eq "" ) {
		die __LINE__ . ": Unable to query section number, verify latex build is up to date"
	    }

	    $replacementText = "(Section $secnum)";
	}

	$comment =~ s/\\ref\{sec:(\S+)/$replacementText/g;
    }

    return( $comment );
} # end check_for_section_replacement()

sub update_cmd {
    my $cmd = shift;

    $cmd =~ s/\(\*\\install\*\)/$Install/;
    $cmd =~ s/\(\*\\chrootinstall\*\)/$chrootInstall/;
    $cmd =~ s/\(\*\\groupinstall\*\)/$groupInstall/;
    $cmd =~ s/\(\*\\groupchrootinstall\*\)/$groupChrootInstall/;

    return( $cmd );
}
