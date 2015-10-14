#!/bin/bash

input=PENROSE_results.tsv
working=$input.working
outdir=individual_bench_data
output=$outdir/penrose_data.tsv

cp $input $working

# Remove square brakcets around parameter
# For tree examples e.g. [1,2]
sed -i -e 's/\[[^,]\+,\([^]]\+\)\]/\1/' $working
# For other examples, e.g. [1]
sed -i -e 's/\[\([^]]\+\)\]/\1/' $working

# Ignore the memory field
cut -f 1,2,3 $working > $output

rm $working

cd $outdir

runhaskell split_results.hs
