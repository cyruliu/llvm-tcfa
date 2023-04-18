#!/usr/bin/perl

use File::Copy;

# sample crontab:
# 0 23 * * * cd /home/ejk/cinference/src; git pull; ./ult; ./harvest --fromdir=/home/ejk/cinference/bench --web=true

######################################################################
use Cwd 'abs_path';
my $CWD = abs_path($file);
my $ANALYSIS_DIR = "$CWD/../bench";
my $CVV_DIR      = "$CWD";
my $WEB_DIR      = '/var/www/html/cv';
my $URL          = 'http://banff.cs.stevens.edu/cinference';
my $verifier     = 'ult'; # or 'cpa'
######################################################################


# parse the arguments to decide
# 1) what verifier to use (ult or cpa)
# 2) which benchmarks to run
my @benches;
my $DOWEB = 0;
my @as = @ARGV;
if ($#as >= 0) {
    # shift off the "--X=Y" args
    while ($#as >= 0 && $as[0] =~ m/^--([^=]+)=(.*)$/) {
        my ($k,$v) = ($1,$2);
        if ($k eq 'fromdir') {
            opendir(DIR, $v) or die "$!";
	    my %tmp;
            while(my $f = readdir(DIR)) {
#                next unless $f =~ /^log-(.+)-[^-]+$/;
                next unless $f =~ /^log-(.+)-(rv|oeA|oeB)$/;
                #print "- will harvest $1\n";
                $tmp{$1} = 1;
            }
            closedir(DIR);
	    @benches = sort keys %tmp;
        } elsif ($k eq 'web') {
            $DOWEB = 1;
        } elsif ($k eq 'verifier') {
            $verifier = $v;
    if($#as >= 0) {
        # Assume that the arg is a list of benches
        if ($as[0] =~ m/,/) {
            @benches = split(',', $as[0]);
        } else {
            @benches = @as;
        }
    }
} else {
    #print "no args. fetching all benches\n";
    my $b = qx{./cvv --benches}; chomp($b);
    @benches = split(',', $b);
}

my %bn2name = ( m => 'Memory', c => 'Counter', ac => 'Accum.', ss3 => 'SimpleSet', 'as' => 'ArrayStack', s => 'OldSimpleSet',
                  ssnf => 'SimpleSet-nf', h => 'Hashtable', 'q' => 'Queue', l => 'List', b => 'BitSet', al => 'Algebra' );

sub texProp {
    my ($p) = @_;
    $p =~ s/s([12])->([a-zA-Z]+)/\\sigma[$2]/g;
    $p =~ s/rho_n_1/v_m/g;
    $p =~ s/rho_n_2/v_n/g;
    $p =~ s/rho_x_1/x_1/g;
    $p =~ s/rho_x_2/x_2/g;
    $p =~ s/rho_y_1/y_1/g;
    $p =~ s/rho_//g;
    $p =~ s/\&\&/\\wedge/g;
    $p =~ s/\!=/\\neq/g;
    $p =~ s/1==1/\\textsf{true}/g;
    $p =~ s/==/=/g;
    return '$'.$p.'$';
}
my @OUT;
push @OUT, "%%% ADT   & Method Names           & Property                  & Expected  & RV result  & RV time   & OEA result & OEA time   & OEB result & OEB time     %% bench\n";
push @OUT, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n";
sub sum_stages {
    my ($times,$results) = @_;
    my ($rvt,$oeAt,$oeBt) = map ($times->{$_},   qw/rv oeA oeB/);
    my ($rvr,$oeAr,$oeBr) = map ($results->{$_}, qw/rv oeA oeB/);
    use Data::Dumper;
    #print Dumper($rvt,$oeAt,$oeBt);
    # cases that did not succeed
    return ('\rTIMEOUT','\rUNKNOWN') if grep(/TIMEOUT/, ($rvt,$oeAt,$oeBt));
    return ('\rMEMOUT', '\rUNKNOWN') if grep(/MEMOUT/,  ($rvr,$oeAr,$oeBr));
    #return ('\rFAIL',   '\rUNKNOWN') if grep(/FAIL/,    ($rvr,$oeAr,$oeBr));

    my $sum_r = '\rUNKNOWN';
    $sum_r = '\rTRUE'  if ($rvr eq 'TRUE'  && $oeAr eq 'TRUE'  && $oeBr eq 'TRUE');
    #$sum_r = '\rFALSE' if ($rvr eq 'FALSE');
    #$sum_r = '\rFALSE' if ($rvr eq 'TRUE'  && $oeAr eq 'FALSE');
    $sum_r = '\rFALSE' if ($rvr eq 'FALSE' || $oeAr eq 'FALSE' || $oeBr eq 'FALSE');

    my @tmp = ($rvt+$oeAt+$oeBt, $sum_r);
    #print Dumper($times,$results,\@tmp);
    return @tmp;
}

sub parse_ult {
    my ($b,$stage) = @_;
    return ('FAIL','FAIL') unless -e "$ANALYSIS_DIR/log-$b-$stage";
    open(F,"$ANALYSIS_DIR/log-$b-$stage") or warn "file $ANALYSIS_DIR/log-$b-$stage - $!";
    my ($time,$result) = ('\rUNKNOWN','UNKNOWN');
    while(<F>) {
        if (/RESULT: Ultimate proved your program to be correct/) { $result = 'TRUE'; }
        if (/RESULT: Ultimate proved your program to be incorrect/) { $result = 'FALSE'; }
        if (/OverallTime: (\d+\.\d+)s,/) { $time = $1; }
        if (/out of memory/) { $result = 'MEMOUT'; }
        if (/Cannot allocate memory/) { $result = 'MEMOUT'; }
    }
    close F;
    #print "$stage - $time - $result\n";
    return ($time,$result);
}
sub parse_cpa {
    my ($b,$stage) = @_;
    return ('FAIL','FAIL') unless -e "$ANALYSIS_DIR/cpa-$b-$stage/Statistics.txt";
    my ($time,$result) = ('\rUNKNOWN','UNKNOWN');
    open(F,"$ANALYSIS_DIR/cpa-$b-$stage/Statistics.txt") or warn "file $ANALYSIS_DIR/cpa-$b-$stage/Statistics.txt - $!";
    while(<F>) {
        while(<F>) {
            if (/Total time for CPAchecker:        ([^s]*)s$/) { $time = $1; push @stage_ts, $1; $total_t += $1; }
            if (/Verification result: TRUE./) { $result = 'TRUE'; }
            if (/Verification result: FALSE./) { $result = 'FALSE'; }
            if (/Verification result: UNKNOWN/) { $result = 'UNKNOWN'; $ok = "\\rFAIL"; }
        }
    }
    close F;
    return ($time,$result);
}


sub fetch_results {
    my ($tool,$b,$expected) = @_;
    my %times; my %results;
    foreach my $stage (qw/rv oeA oeB/) {
        my ($result,$time) = ('STRANGE','STRANGE');
        ($time,$result) = parse_ult($b,$stage) if $tool eq 'ult';
        ($time,$result) = parse_cpa($b,$stage) if $tool eq 'cpa';
        $times{$stage} = $time;
        $results{$stage} = $result;
    }
    #use Data::Dumper; print "TESTINGL".Dumper(\%times,\%results);
    my ($total_time,$total_result) = sum_stages(\%times,\%results);
    # ignore stuff after the first 'FALSE'
    $results{oeA} = 'IG' if $results{rv} eq 'FALSE';
    $results{oeB} = 'IG' if $results{rv} eq 'FALSE' or $results{oeA} eq 'FALSE';
    $times{oeA} = '\rIG' if $results{rv} eq 'FALSE';
    $times{oeB} = '\rIG' if $results{rv} eq 'FALSE' or $results{oeA} eq 'FALSE';
    my $ok = ($total_result eq $expected ? '' : '\rFAIL');
    return sprintf("& %-4s + %-4s + %-4s (\\r%-6s,\\r%-6s,\\r%-6s) & %-6s & %-8s $ok ",
                   map ($times{$_}, qw/rv oeA oeB/),
                   map ($results{$_}, qw/rv oeA oeB/),
                   $total_time, $total_result);
}

foreach my $b (@benches) {
  my ($ds,$m1,$m2,$phi,$expt) = split('-',$b);

  my $failin = substr $expt, 1;
  my $expected = ($expt eq 's' ? '\rTRUE' : '\rFALSE');
  my $bname = $bn2name{$ds};

  my $mets = qx{./cvv --methods $b}; chomp($mets);
  my $prop = qx{./cvv --varphi  $b}; chomp($prop); $prop = texProp($prop);
  my $o = sprintf("%-8s & %-22s & %-25s & %-8s ", $bname, $mets, $prop,  $expected);

  my $cpa_cols = fetch_results('cpa',$b,$expected);
  #my $ult_cols = fetch_results('ult',$b,$expected);

  $o .= "$cpa_cols   \\\\  %% $b\n";
  #$o .=  "$ult_cols   \\\\  %% $b\n";   
  push @OUT, $o;
}


if ($DOWEB == 1) {
    my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)=localtime(time);
    my $ts = sprintf ( "%04d%02d%02d-%02d%02d%02d",
                       $year+1900,$mon+1,$mday,$hour,$min,$sec);
    open OUT, ">$WEB_DIR/results-$ts.txt" or die $!;
    print OUT foreach @OUT;
    close OUT;
    print "results: $URL/results-$ts.txt\n";
    if (-e "$WEB_DIR/log") {
        copy("$WEB_DIR/log", "$WEB_DIR/log-$ts.txt");
        print "log:     $URL/log-$ts.txt\n";
    }
} else {
    print foreach @OUT;
}
