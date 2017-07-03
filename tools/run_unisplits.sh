#!/bin/bash

stack exec output-split-timing >> stats/splits.data.R

cp splits.yaml stats/

pushd stats > /dev/null

rm samples.?.out

truncate -s 0 raw_summary.csv
truncate -s 0 summary.csv

echo -n "Running unisplits..."

for i in {1..5}; do
    ./bin/unisplits sample num_samples=1000 data file=splits.data.R output file=samples.${i}.out > /dev/null
done

echo " Done."

echo -n "Running stansummary..."

./bin/stansummary --sig_figs=6 --csv_file=raw_summary.csv samples.?.out > /dev/null

grep -v \# raw_summary.csv > summary.csv

echo " Done."

echo -n "Updating splits..."

stack exec update-split-timing > splits.data.R

mv splits.yaml ..

echo " Done."

popd > /dev/null
