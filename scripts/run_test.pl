#!/usr/bin/perl
use strict;
use warnings;
use File::Basename;
use File::Temp qw/mkdtemp/;
use File::Copy;

my $ROOT = dirname(__FILE__);
my $BENCHDIR = "$ROOT/examples";
# my $OUTDIR = mkdtemp("$ROOT/out-XXXXXX");
my $OUTDIR = mkdtemp("$ROOT/bit-XXXX");



opendir (DIR, $BENCHDIR) or die $!;
while (my $fn = readdir(DIR)) {
    next unless $fn =~ /\.c$/;
    print "+ $BENCHDIR/$fn\n";
    $fn =~ s/\.c$//;
    mkdir("$OUTDIR/$fn");
    print "  out: $OUTDIR/$fn/\n";
    my $src = "$OUTDIR/$fn/source.c";
    copy("$BENCHDIR/$fn.c", $src);
    my $bin73 = "$OUTDIR/$fn/a.out-gcc73-O2";
    my $src_llvm4 = "$OUTDIR/$fn/a.out-src-llvm4.ll";
    my $veri_llvm4 = "$OUTDIR/$fn/a.out-verifier-llvm4.ll";
    my $link_llvm4 = "$OUTDIR/$fn/a.out-link-llvm4.ll";
    print "+ compile with gcc:";
    print qx{gcc -O2 $ROOT/verifier.c $src -o $bin73};
   
    print "+ compile with llvm:";
    print qx{clang-4.0 -S -emit-llvm $src -o $src_llvm4};
    print qx{clang-4.0 -S -emit-llvm $ROOT/verifier.c -o $veri_llvm4};
    # print qx{clang-link -S $src_llvm4 $veri_llvm4 -o $link_llvm4};
    # print qx{clang-as-4.0 $link_llvm4};

    # my $ir = "$OUTDIR/$fn/ir.dot";
    # my $cfg = "$OUTDIR/$fn/cfg.dot";
    # my $snow = "$OUTDIR/$fn/a.out-gcc73-O3-snow.c";
    # print qx{nocode --print-ir $bin73 > $ir};
    # print qx{nocode --print-cfg $bin73 > $cfg};
    # print qx{nocode --print-cxx $bin73 > $snow};
}
close DIR;


__DATA__
