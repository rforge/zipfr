#!/usr/bin/perl -w
#######################################################################
###                                                                 ###
###   Authors: Stefan Evert and Marco Baroni                        ###
###   Purpose: Compute subcorpus stats (number of types and number  ###
###            of hapaxes, frequency spectra) for a p-attribute in  ###
###            a cwbified corpus, possibly randomized n times by    ###
###            arbitrary s-attribute                                ### 
###                                                                 ###
###   Created: Tue Oct 25 19:12:55 2005                             ###
###   Modified: July 2006 (mbaroni)                                 ###
###   Sorry, this is too orderly for my taste (mb)                  ###
###                                                                 ###
#######################################################################
$| = 1;
use strict "vars";

use PDL;
use CWB;
use CL;
use FileHandle;
use Getopt::Std;
use Time::HiRes qw / gettimeofday /;
use File::Temp qw / tempfile /;

my $usage;
{
$usage = <<"_USAGE_";
This script computes observed vocabulary growth curves and subcorpus
spectra of CWB-encoded corpora, for multiple randomizations by
arbitrary structural attributes (e.g., randomizations at the sentence
level or at the document level). The script can also be used to
generate subcorpus statistics for the non-randomized corpus (together
or in alternative to randomization statistics).

The script creates a directory (by default, subcorpus_data), and a set
of sub-directories inside it where the output data are left (the vgcs
sub-directory contains the vocabulary growth data, and a spectra-X
sub-directory is created for each value X multiplied by N to get the
N0 sizes at which spectra were computed -- e.g., spectra-0.01,
spectra-01, etc.)

The vgc files are in format N V V1 (tab-delimited) with a header
line. The spectrum files are in format m Vm (tab-delimited), also with
a header line.

This script is meant for large experiments and/or large corpora, and
it requires PDL, the CWB-Perl modules and, of course, CWB itself to be
installed.

Usage:  randomization_experiments.pl [options] <CORPUS> [<struct>]

Options:

-p <name>    decode p-attribute <name> [default: word]
-s <seed>    init random number generator with <seed> [default: use srand()]
-d <dir>     generate output files in subdirectories of <dir>
             [default: subcorpus_data]
-r <count>   <count> of randomizations [default: 10]
-o <outtype> output type: vgc, spectrum, both [default: both]
-l <length>  <length> of subsections in number of tokens [default: 10,000]
-n <values>  comma-delimited list of <values> to be multiplied by N to get N0
             sizes at which spectrum is computed [default: .01,.1,.2,.5]
-b           compute also subcorpus stats for non-randomized "basic" corpus
	     (to compute basic stats only, set -r to 0); non-randomized data
             are stored in files named "basic" inside each sub-directory
-v           print some messages to STDERR
-h           print this message and quit

Arguments:

<CORPUS>    corpus name of CWB-ified corpus
<struct>    structural attribute used for randomization (optional and ignored
            if: -b -r 0, mandatory otherwise)

Examples:

100 randomizations of the Brown by sentence, plus the non-randomized
data:

randomization_experiments.pl -b -r 100 BROWN s

10 (= default) randomizations of the Brown by file, printing the vgcs
only:

randomization_experiments.pl -o v BROWN file

Just the non-randomized stats:

randomization_experiments.pl -b -r 0 BROWN

Read this documentation:

randomization_experiments.pl -h | more


Copyright 2006, Marco Baroni and Stefan Evert

This program is free software. You may copy or redistribute it under
the same terms as Perl itself.
_USAGE_
}

my $dir_name;
my $ran_count;
my $sec_length;
my $p_att;
my $out_type = 2;
my @spectra_props = (.01, .1, .2, .5);

# time markers used only in v mode;
my $t0;
my $t1;
# add as needed!

my %opts = ();
getopts('hd:r:l:p:s:o:n:vb',\%opts) or die $usage;

my $basic_only = 0;
if ($opts{b} && ($opts{r} == 0)) {
    $basic_only = 1;
}


if ($opts{h} || (!($basic_only) && (@ARGV != 2)) ||
    ($basic_only && ((@ARGV<1) || (@ARGV>2)) ) ) {
    print $usage;
    exit;
}

if (!($dir_name = $opts{d})) {
    $dir_name = "subcorpus_data";
}

die "Oops! Output directory already exists.\n"
    if -e "$dir_name";

`mkdir $dir_name`;

if ($basic_only) {
    $ran_count = 0;
}
elsif (!($ran_count = $opts{r})) {
    $ran_count = 10;
}

if (!($sec_length = $opts{l})) {
    $sec_length = 10000;
}

if (!($p_att = $opts{p})) {
    $p_att = "word";
}

if (defined($opts{s})) {
    srand $opts{s};
}
else {
    srand();
}

# output types:
# 2 both (default)
# 1 vcgs only
# 0 spectra only
if ($opts{o}) {
    if ($opts{o} =~/^s/) {
	$out_type = 0;
    }
    elsif ($opts{o} =~/^v/) {
	$out_type = 1;
    }
}

# check if user also passed list of comma delimited n0 props for spectra
# (more readable to do it even if out_type == 1, and then do nothing with
# these values, althoug perhaps warning should be sent)
if ($opts{n}) {
    @spectra_props = sort {$a <=> $b} split (",",$opts{n});
} 

# prepare corresponding sub-directories
if ($out_type != 1) {
    foreach (@spectra_props) {
	`mkdir $dir_name/spectra-$_`;
    }
}

# sub-directory for vgcs
if ($out_type > 0) {
    `mkdir $dir_name/vgcs`;
}


if ($opts{v}) {
    $t0 = gettimeofday();
    print STDERR "beginning corpus access setup at $t0\n";
}

my $corpus = uc(shift);
my $s_att = shift;

my $C = new CL::Corpus $corpus
    or die "Can't access corpus $corpus";

my $P = $C->attribute("$p_att", 'p')
    or die "Can't access p-attribute $corpus.$p_att";
my $corpus_length = $P->max_cpos - 1;
my @spectra_n0s = ();
foreach (@spectra_props) {
    push @spectra_n0s, int($corpus_length * $_);
}

if ($opts{v}) {
    print STDERR "N0s are: ", join (", ",@spectra_n0s), "\n";
    print STDERR "corresponding props are: ", join (", ",@spectra_props), "\n";
}

my $S;
my $number_of_regions;
if (!$basic_only) {
    $S = $C->attribute($s_att, 's')
	or die  "Can't access s-attribute $corpus.$s_att";
    $number_of_regions = $S->max_struc; 
}

if ($opts{v}) {
    $t1 = gettimeofday();
    print STDERR "corpus access ready at $t1\n";
    print STDERR "time employed ", $t1-$t0, "\n";
}

my $temp;
my $randomized;
my $tokens;
my $types;
my $hapaxes;
my $curr;
my $i;
my $start;
my $end;
my $word;
my %tok_fq;
my $n0_array_index;

my $rep_counter = 1;
if ($opts{b}) {
    $rep_counter = 0;
}

while ($rep_counter <= $ran_count) {

    if ($rep_counter > 0 ) {
	if ($opts{v}) {
	    $t0 = gettimeofday();
	    print STDERR "random vector generation number $rep_counter started at $t0\n";
	}

	$temp = random $number_of_regions;	# generates one random number for each region
	$randomized = qsorti $temp;	        # sort index for vector $temp = 
           #          random permutation of number 0 .. $number_of_regions - 1
	undef $temp; # just in case

    }
	if ($opts{v}) {
	    if ($rep_counter > 0) {
		$t1 = gettimeofday();
		print STDERR "random vector ready at $t1\n";
		print STDERR "time employed ", $t1-$t0, "\n";
		$t0 = gettimeofday();
		print STDERR "started printing shuffled corpus number $rep_counter at $t0\n";
	    }
	    else {
		$t0 = gettimeofday();
		print STDERR "started printing basic corpus file at $t0\n";
	    }
	}
    

    my ($temphandle,$tempfile) = tempfile(UNLINK => 1, DIR => $dir_name);

    # if this is a randomization,
    # use cwb-decode in matchlist mode (-p) to decode each region,
    # then remove blank separator lines with grep
    
    if ($rep_counter > 0) {
	my $DecodePipe = new FileHandle "| $CWB::Decode -C -p $corpus -P $p_att | grep -v '^\$' > $tempfile"
	    or die "Can't open cwb-decode pipe: $!\n";
  
	# send start and end of each region to the pipe, preceded by a random sort key
	foreach $i (0 .. $number_of_regions-1) {
	    ($start, $end) = $S->struc2cpos($randomized->index($i));
	    print $DecodePipe $start, "\t", $end, "\n"
		or die "Error in cwb-decode pipe: $!\n";
	}
	$DecodePipe->close;
    }
    # else, just print out corpus
    else {
	`cwb-decode -C $corpus -P $p_att > $tempfile`;
    }

    if ($opts{v}) {
	$t1 = gettimeofday();
	print STDERR "shuffling done at $t1\n";
	print STDERR "time employed ", $t1-$t0, "\n";
	$t0 = gettimeofday();
	if ($rep_counter > 0) {
	    print STDERR "started generating output files of iteration $rep_counter at $t0\n";
	}
	else {
	    print STDERR "started generating basic output files at $t0\n";
	}
    }


    $tokens = 0;
    $types = 0;
    $hapaxes = 0;
    %tok_fq = ();

    if ($out_type > 0) {
	if ($rep_counter == 0) {
	    $curr = "basic";
	}
	else {
	    $curr = "randomization." . $rep_counter;
	}
	open CURRVGC,"> $dir_name/vgcs/$curr"
	    or die "at iteration $rep_counter I could not open new output vgc file";
	print CURRVGC "N\tV\tV1\n";
    }
    
    # reset index into n0 array
    $n0_array_index = 0;

    while (<$temphandle>) {
	$word = $_;
    
	$tokens++;
	
	my $f = $tok_fq{$word}++; 

	if ($out_type > 0 ) {   # if we print vcgs
	    if ($f == 0) {
		$types++;
		$hapaxes++;
	    }
	    elsif ($f == 1) {
		$hapaxes--;
	    }

	    if (!($tokens%$sec_length)) {
		print CURRVGC "$tokens\t$types\t$hapaxes\n";
	    }
	}

	if ($out_type != 1) { # if we print spectra
	    if ($tokens == $spectra_n0s[$n0_array_index]) {
		my $spectrum_dir = $dir_name . "/spectra-" . $spectra_props[$n0_array_index];
		my $curr_spc;
		if ($rep_counter > 0) {
		    $curr_spc = $spectrum_dir . "/randomization." . $rep_counter;
		}
		else {
		    $curr_spc = $spectrum_dir . "/basic";
		}
		open CURRSPC, ">$curr_spc"
		    or die "could not open spectrum file $curr_spc";
		print CURRSPC "m\tVm\n";
		my %spectrum = ();
		foreach my $f (values %tok_fq) {
		    $spectrum{$f}++;
		}
		foreach my $m (sort {$a <=> $b} keys %spectrum) {
		    print CURRSPC "$m\t$spectrum{$m}\n";
		}
		close CURRSPC;
		%spectrum = ();
		
		# if we are in spectrum only mode and we printed last spc,
		# we should get out of  loop now!
		if ($n0_array_index == $#spectra_n0s) {
		    if ($out_type == 0) {
			last;
		    }
		}
		# else, increase index
		else {
		    $n0_array_index++;
		}
	    }
	}
    }

    if ($out_type > 0) {
	if ($tokens%$sec_length) {
	    print CURRVGC "$tokens\t$types\t$hapaxes\n";
	}
	close CURRVGC;
    }

    close $temphandle;
    # I should not be doing this, but it looks like in this context
    # temp file is not deleted automatically...
    `rm -f $tempfile`;

    if ($opts{v}) {
	$t1 = gettimeofday();
	if ($rep_counter > 0) {
	    print STDERR "creation of files of round $rep_counter done at $t1\n";
	}
	else {
	    print STDERR "creation of files with basic corpus stats done at $t1\n";
	}
	print STDERR "time employed ", $t1-$t0, "\n";
    }

    undef $randomized;
    $rep_counter++;
}

