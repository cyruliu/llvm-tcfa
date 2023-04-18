#!/usr/bin/perl
#use strict;
#use warnings;

#use HTML::FromANSIl
my ($URLBASE,$STAMP,$RESULTSDIR,$BENCHMARKS) = @ARGV;
my @bench = split ':', $BENCHMARKS;
my $MAXTREES = 2;

sub postprocesstree {
    my ($tree) = @_;
    $tree =~ s/()/$1\\_$2/mg;
    $tree =~ s/(a-zA-Z)_(\d+)/$1_{$2}/mg;
    return $tree;
}

open IDX, ">$RESULTSDIR/termx_summary.html" or die $!;
open EM, ">$RESULTSDIR/EMAIL.txt" or die $!;
open NBENCH, ">$RESULTSDIR/NUMBENCH.tex" or die $!;

#print FULL "\n\n\\begin{center}\\emph{(Begins on next page.)}\\end{center}\n";
#print FULL "\\begin{landscape}\n";
print EM "*** RESULTS OF RUNNING ALL BENCHMARKS ***\n";
printf EM "                               Source Code  |Expecting Result   |Temper Result   |CPAchekcer Result   |GCC -O0   |GCC -O2\n";
print IDX "<html><head><style> table { width:100%; } table, th, td { border: 1px solid black; border-collapse: collapse; } th, td { padding: 15px; text-align: center; } tr:nth-child(even) { background-color: #eee; } tr:nth-child(odd) { background-color: #fff; } </style></head>";
print IDX "<body><h2>Experimental Results of Termx86_$STAMP</h2><table style=\"border: 1pt solid black;\">\n";
print IDX "<tr><th rowspan=\"1\">Benchmark</th><th rowspan=\"1\">Expecting Result</th><th rowspan=\"1\">Temper Result</th><th rowspan=\"1\">CPAchecker Result</th><th rowspan=\"1\">GCC -O0</th><th rowspan=\"1\">GCC -O2</th></tr>\n";



my $i = 0;
my $expect = "";
my $temper = "";
my $opt0 = "";
my $opt2 = "";
foreach my $b (sort @bench) {
    next if $b eq "";
    ++$i;
    my $result_cpa = qx{grep -m 1 "Verification result:" $RESULTSDIR/cpa-$b.txt};
    if ($result_cpa =~ /TRUE/){
        $cpachecker = "TRUE";
    }
    elsif ($result_cpa =~ /FALSE/){
        $cpachecker = "FALSE(cex)";    
    } 
    elsif ($result_cpa =~ /UNKNOWN/){
        $cpachecker = "UNKNOWN";    
    } else {
        $cpachecker = "ERROR";
    }

    my $result_temper = qx{grep -m 1 "Result:" $RESULTSDIR/temper-$b.txt};
    if ($result_temper =~ /TRUE/){
        $temper = "TRUE";
    }
    elsif ($result_temper =~ /FALSE/){
        $temper = "FALSE(cex)";    
    } else {
        $temper = "ERROR";
    }

    if ($b =~ /succeed/){
        $expect = "TRUE";
    }
    elsif ($b =~ /fail/){
        $expect = "FALSE";    
    } else {
        $expect = "UnKown";
    }

    $opt0 = "FALSE(cex)";
    $opt2 = "TURE";

    print IDX "<tr style=\"border-top: 1pt solid black;\"><td><a href=\"src-$b.html\"><pre>$b</pre><td>$expect</td><td><a href=\"temper-$b.html\">$temper</a></td><td><a href=\"cpa-$b.html\">$cpachecker</a></td><td>$opt0</td><td>$opt2</td></tr>\n";

    #  print EM "$b : RESULT : $URLBASE/results-$STAMP/temper-$b.html\n";
   printf EM "  -%50s| %-8s | %8s| %8s| %8s| %8s| \n", $b, $expect, $temper, $cpachecker, $opt0, $opt2;
   #   print EM "$b RESULT : $URLBASE/temper-$b.html\n";
 
}



#print EM "$b : SUMMARY : $URLBASE/results-$STAMP/termx_summary.html\n";
print EM " RESULTS SUMMARY : $URLBASE/termx_summary.html\n";
print IDX "</ul></body></html>\n";
close IDX;
close EM;
my $count = 0 + @bench;
print NBENCH "\\newcommand\\numbenchmarks{$count}\n";
close NBENCH;
