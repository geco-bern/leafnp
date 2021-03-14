#!/bin/bash

# njobs=100
# for ((n=1;n<=${njobs};n++)); do
#     echo "Submitting chunk number $n ..."
#     bsub -W 72:00 -u bestocke -J "ingest_run_rsofun $n" -R "rusage[mem=48000]" "Rscript --vanilla rscript_ingest_run_rsofun.R $n $njobs"
# done

bsub -W 72:00 -u bestocke -J "ingest_run_rsofun 26" -R "rusage[mem=48000]" "Rscript --vanilla rscript_ingest_run_rsofun.R 26 100"
bsub -W 72:00 -u bestocke -J "ingest_run_rsofun 30" -R "rusage[mem=48000]" "Rscript --vanilla rscript_ingest_run_rsofun.R 30 100"
bsub -W 72:00 -u bestocke -J "ingest_run_rsofun 35" -R "rusage[mem=48000]" "Rscript --vanilla rscript_ingest_run_rsofun.R 35 100"
bsub -W 72:00 -u bestocke -J "ingest_run_rsofun 38" -R "rusage[mem=48000]" "Rscript --vanilla rscript_ingest_run_rsofun.R 38 100"
bsub -W 72:00 -u bestocke -J "ingest_run_rsofun 80" -R "rusage[mem=48000]" "Rscript --vanilla rscript_ingest_run_rsofun.R 80 100"
bsub -W 72:00 -u bestocke -J "ingest_run_rsofun 81" -R "rusage[mem=48000]" "Rscript --vanilla rscript_ingest_run_rsofun.R 81 100"
bsub -W 72:00 -u bestocke -J "ingest_run_rsofun 83" -R "rusage[mem=48000]" "Rscript --vanilla rscript_ingest_run_rsofun.R 83 100"