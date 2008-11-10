#!/usr/bin/perl -w
###################################################################
###                                                             ###
###    Author: Stefan Evert                                     ###
###   Purpose: Compute VGC and frequency spectra for BROWN corpus #
###   Created: Thu Mar  7 14:59:08 2002                         ###
###  Modified: Mon Jun 12 06:34:44 2006 (severt)                ###
###                                                             ###
###################################################################
# 
# 
# 
$| = 1;

use CWB;
use CQP;

$corpus = "BROWN";

$Step_Obs = 5_000;		# step width for .obsi file (development of V and V_1, .., V_5)
$Step_Spc = 100_000;		# step width for spectrum and data set computation

$N = 0;
$V = 0;
%Freq = ();			# $Freq{$word} = $f

$fh_obs = CWB::OpenFile "> brown.obsi";
print $fh_obs join("\t", qw<N V V1 V2 V3 V4 V5>), "\n";

$CQP = new CQP;
$CQP->exec("$corpus");
$CQP->exec("set MatchingStrategy traditional");
$CQP->exec("set Context 0");
$CQP->exec("set LeftKWICDelim ''");
$CQP->exec("set RightKWICDelim ''");
$CQP->exec("show +word -cpos");

print "Scanning $corpus ...";
$t = time;
## $CQP->exec('A = [pos = "NN." & word="[a-z][a-z-]+[a-z]"%cd]');
$CQP->exec("A = [pos='[A-Z].+' & pos!='SYM']");
$dt = time - $t;
($corp_N) = $CQP->exec("size A");
print " $corp_N matches [$dt seconds]\n";

print "Processing ";
$t = time;
for ($start = 0; $start < $corp_N; $start += 100) {
  $end = $start + 99;
  $end = $corp_N - 1
    if $end >= $corp_N;
  @lines = $CQP->exec("cat A $start $end");
  foreach $line (@lines) {
    $word = $line;
    $N++;
    if (not exists $Freq{$word}) {
      $V++;
    }
    $Freq{$word}++;

    if (($N % $Step_Obs) == 0) {
      print ".";		# add line to .obsi file
      print $fh_obs "$N\t$V";
      foreach $m (1 .. 5) {
	$Vm = grep {$_ == $m} values %Freq;
	print $fh_obs "\t$Vm";
      }
      print $fh_obs "\n";
    }
    
    if (($N % $Step_Spc) == 0) {
      print "S";		# compute full spectrum 
      $K = int($N / 1000);	# spectrum after $K thousand pairs
      %Spc = ();
      foreach $m (values %Freq) {
	$Spc{$m}++;
      }
      @classes = sort {$a <=> $b} keys %Spc; 
      $fh_spc = CWB::OpenFile "> brown.${K}K.spc";
      print $fh_spc "m\tVm\n";
      foreach $m (@classes) {
	print $fh_spc "$m\t", $Spc{$m}, "\n";
      }
      $fh_spc->close;
      @classes = ();
      %Spc = ();
    }
  }
}
$dt = time - $t;
print " [$dt seconds]\n";

undef $CQP;

$fh_obs->close; 
print "Total size:  N = $N,  V = $V\n";

