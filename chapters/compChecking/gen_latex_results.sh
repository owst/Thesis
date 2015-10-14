TAB="$(printf '\t')"
outfile=results_timemem_slow.tex
infile=penrose_slow.tsv

read -r -d '' TABULARHEADER <<'EOF'
\rowcolors{2}{gray!25}{white}
\begin{tabular}{ | c | c | c | c | }
\hline
Sys & M & T & R? \\ \hline
EOF

read -r -d '' TABULARFOOTER <<'EOF'
\hline
\end{tabular}
EOF

cat << 'EOF' > $outfile
\begin{table}[ht]
\centering
\newcommand{\mycaption}{Checking reachability of markings in \figref{fig:slowmarkings}, using \algref{alg:PNBAlgorithm}}
\caption[\mycaption]{\mycaption. Key: M = Maximum \# States in a Composition, T = Time (s), R? = Reachable?}
\label{tab:slowtimings}
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
renames[buffer-slow]="\\\\bufferSys"
renames[token_ring]="\\\\tokenringSys"
renames[iterated_choice]="\\\\iteratedchoiceSys"
renames[conjunction_tree]="\\\\contree"
renames[disjunction_tree]="\\\\distree"
renames[subsets]="\\\\powersetSys"
renames[cyclic]="\\\\cyclicschedulerSys"

for rename in "${!renames[@]}"
do
    sed -i -e "s/^$rename\s*\([0-9]*\)/${renames[$rename]}{\1}/" $infile
done

# Grab the extra arg of the tree systems
sed -i -e "s/\(tree{[^}]\+}\)\s\([0-9]*\)/\1{\2}/" $infile

sed -e 's/\t/\t\&\t/g' $infile | sed -e 's/$/ \\\\/' >> $outfile

# Replace True/False reachability results with icons
sed -i -e's/True/$\\yesReachable$/' $outfile
sed -i -e's/False/$\\noReachable$/' $outfile

# After the last overtake line, insert temp
# http://unix.stackexchange.com/a/32912/82656 
echo "$TABULARFOOTER" > temp
echo "$TABULARHEADER" >> temp
lastOvertakeLine=$(grep -n overtake $outfile | tail -1 | sed 's/^\([0-9]\+\):.*$/\1/')
sed -i -e "${lastOvertakeLine}r temp" $outfile 
rm temp

echo "$TABULARFOOTER" >> $outfile
cat << 'EOF' >> $outfile
}
\end{table}
EOF
