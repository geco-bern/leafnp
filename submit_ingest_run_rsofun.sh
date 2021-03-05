#!/bin/bash

njobs=500
for ((n=1;n<=${njobs};n++)); do
    echo "Submitting chunk number $n ..."
    bsub -W 72:00 -u bestocke -J "ingest_run_rsofun $n" -R "rusage[mem=48000]" "Rscript --vanilla rscript_ingest_run_rsofun.R $n $njobs"
done
