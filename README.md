# Analysis

Contains all analysis based upon data formatted in (or by scripts in) the data directory.

The main directory contains analysis files, while the plots subdirectory contains graphics based upon the output of these analysis. Obviously run analysis before trying to generate plots.

Order of execution (all in subdirectory `./vignettes/`):

1. Collect data: `vignettes/leafnp.Rmd`
2. Feature elimination: `src/submit_feature_elimination_leafnp.sh` and `analysis/feature_elimination_leafnp.R`. (t-values from LMMs in `vignettes/model_fitting.Rmd`)
3. Model fitting: `vignettes/model_fitting.Rmd`
4. Trait gradient analysis: `vignettes/traitgradient.Rmd`

(Other files are not used anymore)