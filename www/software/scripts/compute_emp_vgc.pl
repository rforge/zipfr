#!/usr/bin/perl -w

# this assumes a corpus in one word per line format and computes
# number of types and number of hapaxes 
# usage:
# compute_emp_vgc.pl size_of_section corpus

use Getopt::Std;

my $usage;
{
$usage = <<"_USAGE_";
This script computes observed vocabulary and hapax growth curves for
corpora in one-line-per-token format.

The output (sent to STDOUT) is in tab-delimited N V V1 format, with a
header line.

By default, V and V1 are computed every 10,000 tokens (and at the full
corpus size), but the distance between Ns at which V and V1 are
computed can be controlled with the option -l.

Usage:  compute_emp_vgc.pl [options] <CORPUS>

Options and argument:

-l <length>  <length> of sections in number of tokens [default: 10,000]
-h           print this message and quit

<CORPUS>    corpus in one-token-per-line format


Examples:

Default (V/V1 counts separated by 10,000 tokens):

compute_emp_vgc.pl input > vgc

More samples:

compute_emp_vgc.pl -l 1000 input > vgc

In a pipe (read data from STDIN):

compute_emp_vgc.pl - > vgc

Read this documentation:

compute_emp_vgc.pl -h | more


Copyright 2006, Marco Baroni and Stefan Evert

This program is free software. You may copy or redistribute it under
the same terms as Perl itself.
_USAGE_
}


%opts = ();
getopts('hl:',\%opts) or die $usage;

if ($opts{h} || (@ARGV != 1)) {
    print $usage;
    exit;
}


if (!($sectionsize = $opts{l})) {
    $sectionsize = 10000;
}

$tokens = 0;


print "N\tV\tV1\n";

open CORPUS,shift or die "could not find input corpus";
while (<CORPUS>) {
    chomp;
    $wf = $_;

    $tokens++;
    
    if (!$tok_fq{$wf}) {
	$types++;
    }
    $tok_fq{$wf}++;

    if (!($tokens%$sectionsize)) {

	$hapaxes = count_hapaxes(\%tok_fq);

	print "$tokens\t$types\t$hapaxes\n";
    }
}

if ($tokens%$sectionsize) {

	$hapaxes = count_hapaxes(\%tok_fq);

	print "$tokens\t$types\t$hapaxes\n";

}

sub count_hapaxes {
    my $tf_hash_ref = shift;

    my $hapaxes = 0;

    foreach my $fq (values %$tf_hash_ref) {
	if ($fq == 1) {
	    $hapaxes++;
	}
    }

    return $hapaxes;
}
