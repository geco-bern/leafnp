#!/bin/bash

bsub -W 72:00 -u bestocke -J "feature_elimination_leafnp" -R "rusage[mem=48000]" "Rscript --vanilla feature_elimination_leafnp.R"
