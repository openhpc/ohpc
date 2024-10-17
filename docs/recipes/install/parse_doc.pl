#!/usr/bin/env perl
#
# Simple parser to cull command-line instructions from documentation for
# validation.
#
# v1.0 karl.w.schulz@intel.com
# v2.0 john.a.westlund@intel.com
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
    print STDERR "  --repo     Use a different repo for install commands\n";
    print STDERR "  --ci_run   run additional 'CI only' commands\n";
    exit 1;
}

my $file        = shift;
my $inputDir    = dirname(File::Spec->rel2abs($file));
my $basename    = basename($file,".tex");
chdir $inputDir;

# Determine BaseOS, arch, and define package manager commands
my $BaseOS             = "";
my $BaseOSshort        = "";
my $OSimage            = "";
my $arch               = "";
my $Install            = "";
my $chrootInstall      = "";
my $groupInstall       = "";
my $groupChrootInstall = "";
my $remove             = "";
my $addrepo            = "";
my $chrootaddrepo      = "";
my $beegfsrepo         = "";
my $verlong            = "";
my $OSTree             = "";
my $image              = "";

# parse package command macro's from input file
open( IN, "<$basename.tex" ) || die __LINE__ . ": Cannot open file -> $file\n$!";
while( my $line = <IN> ) {
    chomp( $line );
    if( $line =~ /\\newcommand\{\\install\}\{(.+)\}/ ) {
        $Install = $1;
    } elsif( $line =~ /\\newcommand\{\\chrootinstall\}\{(.+)\}/ ) {
        $chrootInstall = $1;
    } elsif( $line =~ /\\newcommand\{\\groupinstall\}\{(.+)\}/ ) {
        $groupInstall = $1;
    } elsif( $line =~ /\\newcommand\{\\addrepo\}\{(.+)\}/ ) {
        $addrepo = $1;
    } elsif( $line =~ /\\newcommand\{\\chrootaddrepo\}\{(.+)\}/ ) {
        $chrootaddrepo = $1;
    } elsif( $line =~ /\\newcommand\{\\beegfsrepo\}\{(.+)\}/ ) {
        $beegfsrepo = $1;
	# undo latex escape for underscore
	$beegfsrepo =~ s/\\_/_/;
    } elsif( $line =~ /\\newcommand\{\\VERLONG\}\{(.+)\}/ ) {
        $verlong = $1;
    } elsif( $line =~ /\\newcommand\{\\OSTree\}\{(.+)\}/ ) {
        $OSTree = $1;
	# undo latex escape for underscore
	$OSTree =~ s/\\_/_/;
    } elsif( $line =~ /\\newcommand\{\\installimage\}\{(.+)\}/ ) {
        $image = $1;
    } elsif( $line =~ /\\newcommand\{\\groupchrootinstall\}\{(.+)\}/ ) {
        $groupChrootInstall = $1;
    } elsif( $line =~ /\\newcommand\{\\baseos\}\{(.+)\}/ ) {
        $BaseOS = $1;
    } elsif( $line =~ /\\newcommand\{\\baseosshort\}\{(.+)\}/ ) {
        $BaseOSshort = $1;
    } elsif( $line =~ /\\newcommand\{\\osimage\}\{(.+)\}/ ) {
        $OSimage = $1;
    } elsif( $line =~ /\\newcommand\{\\remove\}\{(.+)\}/ ) {
        $remove = $1;
    } elsif( $line =~ /\\newcommand\{\\arch\}\{(.+)\}/ ) {
        $arch = $1;
	# undo latex escape for x86
	if ($arch eq "x86\\_64") { $arch = "x86_64";}
    }
}
close( IN );

# Strip escape \ from latex macro
$chrootaddrepo      =~ s/\\\$/\$/;
$chrootInstall      =~ s/\\\$/\$/;
$groupChrootInstall =~ s/\\\$/\$/;

# print "Install             = $Install\n";
# print "chrootInstall       = $chrootInstall\n";
# print "groupInstall        = $groupInstall\n";
# print "groupChrootInstall  = $groupChrootInstall\n";
# print "BaseOS              = $BaseOS\n";
# print "VERLONG             = $verlong\n";
# print "OSTree              = $OSTree\n";
# print "IMAGE               = $image\n";
# exit(1);

if( $Install eq "" || $chrootInstall eq "" || $groupInstall eq "" || $groupChrootInstall eq "" ) {
    print "Error: package manager macros not defined\n";
    exit 1;
}

# The input .tex file likely includes other input files via \input{foo}.  Do a
# first pass through the input file and accumulate a temp file which includes
# an aggregate copy of all the tex commands.

( my $TMPFH_AGGR_STEPS, my $tmpfile_aggr_steps ) = tempfile();
open( my $INPUT_FH, "<$basename.tex" ) || die __LINE__ . ": Cannot open file -> $file\n$!";
parse_includes( $TMPFH_AGGR_STEPS, $INPUT_FH );
close( $INPUT_FH );
close( $TMPFH_AGGR_STEPS );

# Next, parse the raw file and look for commands to execute based on delimiter
( my $fh, my $tmpfile ) = tempfile();

open( IN, "<$tmpfile_aggr_steps" ) || die __LINE__ . ": Cannot open file -> $file\n$!";

my $begin_delim   = "% begin_ohpc_run";
my $end_delim     = "% end_ohpc_run";
my $prompt        = quotemeta "\[sms\]\(\*\\\#\*\)";
my $psql_prompt   = quotemeta "\[postgres\]\$";
my $disable_delim = "^%%";
my $indent        = 0;

print $fh "#!/bin/bash\n";

while( <IN> ) {
    next if( $_ =~ /$disable_delim/ );

    if( /$begin_delim/../$end_delim/ ) {
        if( $_ =~ /% ohpc_validation_newline/ ) {
            print $fh "\n";
        } elsif( $_ =~ /% ohpc_ci_comment (.+)/ ) {
            next if ( !$ci_run );
            print $fh "# $1 (CI only)\n";
        } elsif( $_ =~ /% ohpc_comment_header (.+)/ ) {
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

        # if line starts a HERE document: prompt$ command <<- HERE > /tmp/foo
        # <<- indicates the HERE document will ignore leadings tabs (not spaces)
        } elsif( $_ =~ /$prompt (.+ <<-[ ]*([^ ]+).*)$/ ) {
            my $cmd  = update_cmd($1);
            chomp $cmd;
            my $here = $2;
            chomp $here;

            # commands that begin with a % are for CI only
            next if( $_ =~ /^%/ && !$ci_run );

            print $fh ' ' x $indent . "$cmd\n";
            my $next_line;
            do {
                $next_line = <IN>;
                # trim leading and trailing space
                $next_line =~ s/^\s+|\s+$//g;

                print $fh "$next_line\n";
            } while( $next_line !~ /^$here/ );

        # handle commands line line continuation: prompt$ command \
        } elsif( $_ =~ /$prompt (.+) \\$/ ) {
            my $cmd = update_cmd( $1 );

            # commands that begin with a % are for CI only
            next if( $_ =~ /^%/ && !$ci_run );

            print $fh ' ' x $indent . "$cmd";
            my $next_line;
            do {
                $next_line = <IN>;
                # trim leading and trailing space
                $next_line =~ s/^\s+|\s+$//g;

                print $fh " $next_line\n";
            } while( $next_line =~ / \\$/ );

        # special treatment for do loops
        } elsif( $_ =~ /$prompt (.+[ ]*;[ ]*do)$/) {
            my $cmd = update_cmd($1);

            print $fh ' ' x $indent . "$cmd\n";
            my $next_line;
            my $local_indent = $indent + 3; # further indent contents of loop
        do {
            $next_line = <IN>;
            # trim leading and trailing space
            $next_line =~ s/^\s+|\s+$//g;

            # reset indent to previous value for the last line
            $local_indent = $indent if( $next_line =~ m/[^#]*done/ );

            printf $fh ' ' x $local_indent . "%s\n",$next_line;
        } while( $next_line !~ m/[^#]*done/ ); # loop until we see an uncommented done

        # normal single line command
        } elsif( $_ =~ /$prompt (.+)$/ ) {
            my $cmd = update_cmd($1);
            # commands that begin with a % are for CI only
            next if( $_ =~ /^%/ && !$ci_run );
            print $fh ' ' x $indent . "$cmd\n";

        # postgres command
        } elsif( $_ =~ /$psql_prompt (.+)$/ ) {
            my $cmd = update_cmd( $1 );
            # commands that begin with a % are for CI only
            next if( $_ =~ /^%/ && !$ci_run );
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
    $cmd =~ s/\(\*\\remove\*\)/$remove/;
    $cmd =~ s/\(\*\\addrepo\*\)/$addrepo/;
    $cmd =~ s/\(\*\\chrootaddrepo\*\)/$chrootaddrepo/;
    $cmd =~ s/\(\*\\beegfsrepo\*\)/$beegfsrepo/;
    $cmd =~ s/BOSVER/$BaseOS/;
    $cmd =~ s/OSIMAGE/$OSimage/;
    $cmd =~ s/BOSSHORT/$BaseOSshort/;
    $cmd =~ s/ARCH/$arch/;
    $cmd =~ s/VERLONG/$verlong/g;
    $cmd =~ s/OSTREE/$OSTree/;
    $cmd =~ s/IMAGE/$image/;

    return( $cmd );
} # end update_cmd

sub parse_includes {
    # OUTFILE is a filehandle all aggregated latex goes to
    # INFILE is the latex filehandle to be read
    my ( $OUTFILE, $INFILE ) = @_;

    while( my $line = <$INFILE> ) {
        # Check for include of another latex file
        if( $line =~ /^\\input\{(\S+)\}/ ) {
            next if( $line =~ /^%%/ );
            my $include_file = $1;

            # verify input file has .tex extension or add it
            if( $include_file !~ /(\S+).tex/ ) {
                $include_file = $include_file . ".tex";
            }
            # open the new latex file and pass it back to this function again with the same outfile
            open( my $INCLUDE_FILE, "<$include_file" ) || die __LINE__ . ": Cannot open file -> $include_file\n$!";
            parse_includes( $OUTFILE, $INCLUDE_FILE );
            close( $INCLUDE_FILE);

        } elsif( $line !~ /^%%/ ) {
            print {$OUTFILE} $line;
        }
    }
} # end parse_includes
