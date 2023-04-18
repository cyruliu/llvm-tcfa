#!/bin/bash

timeout=10
interval=2
delay=5
# set -x

# find root project path from this script path
#dir=$(realpath -L $(dirname $0)/../)
bindir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

#export cpadir=$bindir/../../../../Downloads/Tool/cpachecker
export cpadir=$bindir/../../../../Downloads/cpa-bam-bnb
#URLBASE="file://$bindir/.."
URLBASE="https://cyruliu.github.io/termx/results/"
echo $cpadir

# benchmarks
export benchdir=$bindir/../examples/simple2
cd $benchdir
git pull
EXAMPLES=$(ls *.c) # 00*.c 01*.c 02*.c) # 01sendrecv.c)
# BCS=$(ls *.bc)

export STAMP=`date +'%Y%m%d-%H%M%S'`
export RESULTSDIR=$bindir/../results-$STAMP

#################################
# Run the benchamrks
cd $bindir/../
rm -r results-*

mkdir -p $RESULTSDIR
chmod 755 $RESULTSDIR
chmod +x $bindir/ansi2html.sh

# Run c bechmarks
EXS=""
cd $bindir/../
for ex in $EXAMPLES; do
    EXS=$ex:$EXS
    echo "$EXS"
    cat $benchdir/$ex | $bindir/ansi2html.sh > $RESULTSDIR/src-$ex.html

    # run over temper
    ./deps/temper/main/temper.native -cpa $benchdir/$ex >$RESULTSDIR/temper-$ex.txt
    cat $RESULTSDIR/temper-$ex.txt | $bindir/ansi2html.sh > $RESULTSDIR/temper-$ex.html


# run over cpachecker
    #$cpadir/scripts/cpa.sh -preprocess -timelimit 600s -spec ~/avta/cpathriftserver/src/main/resource/error_reach.prp -config $cpadir/config/predicateAnalysis.properties $benchdir/$ex >$RESULTSDIR/cpa-$ex.txt
    $cpadir/scripts/cpa.sh -preprocess -svcomp19-bam-bnb -heap 10000M -timelimit 600s $benchdir/$ex >$RESULTSDIR/cpa-$ex.txt
    cat $RESULTSDIR/cpa-$ex.txt | $bindir/ansi2html.sh > $RESULTSDIR/cpa-$ex.html


done

: '
# Run McSema llvm bechmarks over llvm-tcfa
BTC=""
cd $bindir/../
for bx in $BCS; do
    BTC=$bx:$BCS
    echo $BTC
    ./llvm_tcfa.native examples/simple2/$bx | grep -m 1 "result: " >$RESULTSDIR/llvm_tcfa-$bx.txt
    cat $RESULTSDIR/llvm_tcfa-$bx.txt | $bindir/ansi2html.sh > $RESULTSDIR/llvm_tcfa-$ex.html
done
'




#################################
# Generate report
chmod +x $bindir/termx_report.pl
$bindir/termx_report.pl $URLBASE $STAMP $RESULTSDIR $EXS

# push the results to git repo
cp -r $RESULTSDIR/* $bindir/../../../termx/results/
cd $bindir/../../../termx/results/
pwd
git pull
git add .
git commit -m "daliy running results-$STAMP"
git push

#set +x
echo ""
echo ""
echo ""
cat $RESULTSDIR/EMAIL.txt
#cat $RESULTSDIR/RESULTS.tex
echo ""
echo "Output saved to: $RESULTSDIR"
echo ""

#sendig email to the group through server

#cat $RESULTSDIR/EMAIL.txt | mail -s "Daily Termx86 Run" ekoskine@stevens.edu, tchanhle@stevens.edu, jxu69@stevens.edu, pangbin2415@gmail.com, yliu195@stevens.edu

: '
LATEST=$(ls -tr /var/www/html/knotical/daily/ | tail -n 1)
RESULTSDIR=/var/www/html/knotical/daily/$LATEST

scp $RESULTSDIR/EMAIL.txt cyrus@www.avta.com:tmp/
ssh cyrus@www.avta.com "cat tmp/EMAIL.txt | mail -s \"Daily Termx86 Run\" avta@lists.stevens.edu"
#ssh erickoskinen@www.erickoskinen.com "cat tmp/knmsg.txt | mail -s \"Daily Bowtie Run\" tchanhle@stevens.edu"
#ssh erickoskinen@www.erickoskinen.com "cat tmp/knmsg.txt | mail -s \"Daily Bowtie Run\" yliu195@stevens.edu"
'

