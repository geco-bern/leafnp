#!/bin/bash

bsub -W 72:00 -u bestocke -J "feature_elimination_leafnp leafN" -R "rusage[mem=48000]" "Rscript --vanilla feature_elimination_leafnp.R leafN"
bsub -W 72:00 -u bestocke -J "feature_elimination_leafnp leafP" -R "rusage[mem=48000]" "Rscript --vanilla feature_elimination_leafnp.R leafP"
bsub -W 72:00 -u bestocke -J "feature_elimination_leafnp LeafNP" -R "rusage[mem=48000]" "Rscript --vanilla feature_elimination_leafnp.R LeafNP"
