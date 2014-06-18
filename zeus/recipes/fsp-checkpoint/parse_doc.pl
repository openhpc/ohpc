#!/usr/bin/env perl
# 
# Simple parser to cull command-line instructions from documentation for
# validation.

use warnings;
use strict;

if ( $#ARGV < 0) {
    print "Usage: parse_doc <filename>\n";
    exit 1;
}

my $file = shift;

#print "Parsing commands from $file\n";

open(IN,"<$file")  || die "Cannot open file -> $file\n";

my $begin_delim = "% begin_fsp_run";
my $end_delim   = "% end_fsp_run";
my $prompt      = "\[master\]\$";

print "#!/bin/bash\n";

while(<IN>) {
    if(/$begin_delim/../$end_delim/) {
	if ($_ =~ /\[master\]\$ (.+)/) {
	    print "$1\n";
	}
    }
}


close(IN);
