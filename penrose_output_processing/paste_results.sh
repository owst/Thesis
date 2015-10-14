DIRNAME=$0_dir
mkdir $DIRNAME

cp *_results.tsv $DIRNAME

cd $DIRNAME

PASTED=pasted.txt
# Ignore the name/size columns for everything except PENROSE
# Sort into order of best->worst for Penrose
paste -d \\t <(cat PENROSE_results.tsv) <(cut -f 3- CLP_results.tsv) <(cut -f 3- CNA_results.tsv) <(cut -f 3- LOLA_results.tsv) <(cut -f 3- TAAPL_results.tsv)  <(cut -f 3- MARCIE_results.tsv) > $PASTED

SORTED=results.tsv

cat $PASTED | runhaskell ../sortResults.hs > $SORTED
rm $PASTED

# Replace [123] with 123 and [4,5] with 4,5
sed -e 's/\[\([0-9,]\+\)\]/\1/' -i $SORTED

sed -e 's/iterated_choice/iter-choice/' -i $SORTED
sed -e 's/token_ring/token-ring/' -i $SORTED
sed -e 's/disjunction_tree/\\distree{-}{-}/' -i $SORTED
sed -e 's/conjunction_tree/\\contree{-}{-}/' -i $SORTED
sed -e 's/FAIL: TimeOut/\\timeoutResult/g' -i $SORTED
sed -e 's/FAIL: OOM/\\oomResult/g' -i $SORTED
sed -e 's/FAIL: WrongResult/\\incorrectResult/g' -i $SORTED
sed -e 's/FAIL: CantHandleQueryPorts/\\queryPortsUnhandledResult/g' -i $SORTED

## Remove problem size 2
#awk '$2!=2' results.tsv > tmp
#mv tmp results.tsv

TAB="$(printf '\t')"

AMP="${TAB}&${TAB}"
AMPESC="${TAB}\&${TAB}"

makeheader(){
cat << EOF
\\rowcolors{2}{gray!25}{white}
\\begin{tabular}{ | c | c || c | c | c | c | c | c | }
\\hline
\\multicolumn{2}{|c||}{Problem}&\\multicolumn{6}{c|}{$1} \\\\
\\hline
name${AMP}size${AMP}Penrose${AMP}CLP${AMP}CNA${AMP}LOLA${AMP}TAAPL${AMP}MARCIE\\\\ \\hline
EOF
}

makefooter(){
cat << EOF
\\hline
\\end{tabular}
EOF
}

# Cut can't handle out-of-order columns, so we do it manually. Place all the
# time columns together and all the mem columns together.
cut -f 1,2 $SORTED > problems
cut -f 3,5,7,9,11,13 $SORTED > time
cut -f 4,6,8,10,12,14 $SORTED > memory

# Mark non-corbett problems
sed -i -e 's/\(iter-choice\|replicator\|counter\)/\\nonCorbett{\1}/' problems

# Replace tabs with latex column separators, and add latex EOLs.
replaceWithLatex() {
sed -e "s/\t/$AMPESC/g" | sed -e 's/$/ \\\\/'
}

pasteProblemsWith() {
# Haskell hack to highlight the minimum value of a column of numbers
paste <(cat problems) <(cat $1 | runhaskell ../highlightMin.hs)
}

delimit_examples(){
    awk 'BEGIN {P=""} { if ($1 != P && P != "") { print "\\hline" }; P = $1; print $0; }' $1
}

go () {
makeheader "$1" > tmp
cp tmp tmp1

#split problems at counter, the first bad example for Penrose
pasteProblemsWith $2 | replaceWithLatex | csplit -f split -n 1 - /counter/

delimit_examples split0 >> tmp
delimit_examples split1 >> tmp1

makefooter >> tmp
makefooter >> tmp1
mv tmp results_$2_1.tex
mv tmp1 results_$2_2.tex
}

go 'Time (s)' time
go 'Max Resident (MB)' memory

mv results_*{1,2}.tex ~/Dropbox/PhD/thesis/chapters/comparisonAndDiscussion

cd ..
rm -r $DIRNAME
