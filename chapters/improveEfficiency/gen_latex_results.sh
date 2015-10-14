TAB="$(printf '\t')"
outfile=results_timemem.tex
infile=PENROSE_results.tsv
workfile=$infile.work

cp $infile $workfile

read -r -d '' TABULARHEADER <<'EOF'
\rowcolors{2}{gray!25}{white}
\begin{tabular}{ | c | c | c | }
\hline
Sys & T & M \\ \hline
EOF

read -r -d '' TABULARFOOTER <<'EOF'
\hline
\end{tabular}
EOF

cat << 'EOF' > $outfile
\begin{table}[ht]
\centering
\newcommand{\mycaption}{Checking reachability of markings in
\figref{fig:slowmarkings}, on examples of \secref{sec:tweakedExamples}, using
\algref{alg:thealgorithm}}
\caption[\mycaption]{\mycaption. Key: T = Time (s), M = Maximum Resident Memory
(MB), \emph{TO} = Time Out of 300s}
\label{tab:fasttimings}
\makebox[\textwidth][c]{
EOF

echo "$TABULARHEADER" >> $outfile

declare -A renames
renames[dac]="\\\\DACSys"
renames[replicator]="\\\\replicatorsSys"
renames[over]="\\\\overtakeSys"
renames[philo]="\\\\diningphilosophersSys"
renames[counter]="\\\\counterSys"
renames[clique]="\\\\cliqueSys"
renames[hartstone]="\\\\hartstoneSys"
renames[buffer]="\\\\bufferSys"
renames[token_ring]="\\\\tokenringSys"
renames[iterated_choice]="\\\\iteratedchoiceSys"
renames[conjunction_tree]="\\\\contree"
renames[disjunction_tree]="\\\\distree"
renames[subsets]="\\\\powersetSys"
renames[cyclic]="\\\\cyclicschedulerSys"

for rename in "${!renames[@]}"
do
    sed -i -e "s/^$rename\s*\[\([0-9]*\)\(,\|\]\)/${renames[$rename]}{\1}/" $workfile
done

# Grab the extra arg of the tree systems
sed -i -e "s/\(tree{[^}]\+}\)\([0-9]*\)\]/\1{\2}/" $workfile

sed -e 's/\t/\t\&\t/g' $workfile | sed -e 's/$/ \\\\/' >> $outfile

# Replace fail string with shorter
sed -i -e's/FAIL: TimeOut/\\emph{TO}/g' $outfile

# After the last overtake line, insert temp
# http://unix.stackexchange.com/a/32912/82656 
echo "$TABULARFOOTER" > temp
echo "$TABULARHEADER" >> temp
lastIterChoiceLine=$(grep -n iteratedchoice $outfile | tail -1 | sed 's/^\([0-9]\+\):.*$/\1/')
sed -i -e "${lastIterChoiceLine}r temp" $outfile 
rm temp

echo "$TABULARFOOTER" >> $outfile
cat << 'EOF' >> $outfile
}
\end{table}
EOF

rm $workfile
