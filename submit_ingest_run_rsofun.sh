#!/bin/bash

njobs=100
for ((n=1;n<=${njobs};n++)); do
    echo "Submitting chunk number $n ..."
    bsub -W 72:00 -u bestocke -J "ingest_run_rsofun $n" -R "rusage[mem=48000]" "Rscript --vanilla rscript_ingest_run_rsofun.R $n $njobs"
done

# bsub -W 72:00 -u bestocke -J "ingest_run_rsofun 44" -R "rusage[mem=48000]" "Rscript --vanilla rscript_ingest_run_rsofun.R 44 100"