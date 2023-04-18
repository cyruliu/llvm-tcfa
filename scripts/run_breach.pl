#!/usr/bin/perl

######################################################################
use Cwd 'abs_path';
my $CWD = abs_path($file);
my $ANALYSIS_DIR = "$CWD/../bench";
my $ULTIMATE_DIR = "/home/ejk/ultimate-dev/ultimate/releaseScripts/default/UAutomizer-linux";
my $REACHPRP     = "$CWD/reach.prp";
my $CPA_DIR      = "~/cpachecker"; # ~/CPAchecker-1.8-unix
my $CVV_DIR      = "$CWD";
my $verifier = 'cpa';
my $TIMEOUT = 2.0 * 60 * 60;  # 7200s=2hr
######################################################################

my %bn2source = ( m => 'mem', ac => 'accumulator', c => 'counter', s => 'ss', ssnf => 'ssnf', ss3 => 'simpleset', 'as' => 'stack', q => 'queue', h => 'hashtable', l => 'list', b => 'bitset', al => 'algebra', par => 'par' );

# decide which benchmarks
my @benches;
my $HARVEST = 0;
my $CPA = 0;
my $PID;
if ($#ARGV >=0 && $ARGV[0] =~ /^--cpa$/) { $verifier = 'cpa'; shift @ARGV; }
if ($#ARGV >=0 && $ARGV[0] =~ /^--ult$/) { $verifier = 'ult'; shift @ARGV; }
if ($#ARGV >=0 && $ARGV[0] =~ /^--andharvest=(.*)$/) {
    $HARVEST = 1;
    $PID = $1;
    shift @ARGV;
}
if ($#ARGV >= 0) {
  if ($ARGV[0] =~ m/,/) {
    @benches = split(',', $ARGV[0]);
  } else {
    @benches = @ARGV;
  }
} else {
  my $b = qx{./cvv --benches}; chomp($b);
  @benches = split(',', $b);
}

unlink("stats.tex");
foreach my $v (@benches) {
    my ($pref,$m1,$m2,$phi,$expect) = split('-',$v);

    print "######################################## BENCHMARK: $v\n";
    foreach my $stage (qw/rv oeA oeB/) {
	# Consruct the benchmark
	chdir($CVV_DIR);
	my $fn = "$verifier.$v.$stage.c";
	my $out = qx{./cvv $verifier $v $stage > $fn};
	my $osrc = $bn2source{$pref};

	print qx{cat common.h ${osrc}.h ${osrc}.c $fn > $ANALYSIS_DIR/$fn.cat.c};
	print qx{gcc -E $ANALYSIS_DIR/$fn.cat.c > $ANALYSIS_DIR/$fn};
	print "---[ $v , stage=$stage ]---\nsource: $fn\n";

	if($verifier eq 'cpa') {
	    # Execute CPAchecker
	    chdir($CPA_DIR);
	    my $OPTIONS="-setprop solver.solver=Z3 -setprop limits.time.cpu=90000000";
	    my $CONFIG="$CPA_DIR/config/predicateAnalysis.properties";
	    my $cmd = "$CPA_DIR/scripts/cpa.sh -config $CONFIG $OPTIONS -skipRecursion "
		."-outputpath $ANALYSIS_DIR/cpa-$v-$stage $ANALYSIS_DIR/$fn";
	    print qq{CMD: $cmd\n};
	    print qx{timeout $TIMEOUT $cmd};
	    
	    my $result = qx{grep "Verification result:" $ANALYSIS_DIR/cpa-$v-$stage/Statistics.txt};
	    if ($result =~ /: TRUE./) { print "---> cpa proof for stage $stage\n"; }
	    elsif ($result =~ /: FALSE./) { print "---> cpa cex for stage $stage\n"; last }
	    elsif ($result =~ /: UNKNOWN./) { print "---> cpa UNKNOWN for stage $stage\n"; }
      else { print "--> cpa STRANGE -- probably a timeout - for $v-$stage\n" }

	} elsif ($verifier eq 'ult') {
	    # Execute Ultimate
	    chdir($ULTIMATE_DIR);
	    # removed "-ea"
	    $cmd = "./Ultimate.py --spec $REACHPRP --architecture 32bit --file $ANALYSIS_DIR/$fn";
	    $cmd = $cmd." --traceabstraction.trace.refinement.strategy BADGER" if ($stage eq 'rv' && grep($v,$needWOLF));
	    print qq{CMD: $cmd\n};
	    print qx{timeout $TIMEOUT $cmd\n};
	    my $result = qx{grep 'RESULT' $ULTIMATE_DIR/Ultimate.log};
	    my $memout = qx{grep 'Cannot allocate memory' $ULTIMATE_DIR/Ultimate.log};
	    rename("$ULTIMATE_DIR/Ultimate.log", "$ANALYSIS_DIR/log-$v-$stage");
	    #print $result;
	    if ($result =~ /incorrect/) { print "---> ult cex for stage $stage\n"; last }
	    elsif ($result =~/to be correct/) { print "---> ult proof for stage $stage\n"; }
	    elsif ($result =~ /UNKNOWN/) { print "---> ult UNKNOWN for stage $stage\n"; }
	    elsif ($memout =~ /Cannot allocate memory/) { print "---> ult MEMOUT for stage $stage\n"; }
	    else { print "--> ult STRANGE -- probably a timeout - for $v-$stage\n" }

	} else { die "unknown verifier $verifier"; }
	
    }

}

my $bms = join(" ",@benches);
if ($HARVEST == 1) {
    chdir($CVV_DIR);
    print "now running harvest: $CVV_DIR/harvest --verifier=cpa --web=true $bms\n";
    print qx{$CVV_DIR/harvest --verifier=cpa --web=true $bms};
    unlink($PID);
} else {
    print "now harvest the output:\n  ./harvest --verifier=cpa $bms\n";
}
