# Analysis

Contains all analysis based upon data formatted in (or by scripts in) the data directory.

The main directory contains analysis files, while the plots subdirectory contains graphics based upon the output of these analysis. Obviously run analysis before trying to generate plots.

Order of execution:

1. Collect data: `leafnp.Rmd`
2. feature elimination: `feature_elimination_leafnp.Rmd`
3. Random forest model fitting: `randomforest_leafnp.Rmd`
4. Trait gradient analysis: `traitgradient.Rmd`
5. Spatial upscaling: ???
