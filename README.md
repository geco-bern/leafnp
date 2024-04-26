# leafnp

This repository contains R code for all analyses and modelling used for the following article:

*Di Tian, Zhengbing Yan, Bernhard Schmid, Jens Kattge, Jingyun Fang, Benjamin D. Stocker: Environmental versus phylogenetic controls on leaf nitrogen and phosphorous concentrations of terrestrial plants. (in review)*

## Preparation

The analysis uses the published dataset of leaf N, P, and N:P concentrations by Tian et al (2019), complemented with environmental covariate data for the present analysis. The complemented dataset and code for generating it in contained in the dataset repository [leafnp_data](https://github.com/geco-bern/leafnp_data).

Download the file and adjust the paths to your local copy of the data file in vignettes (`vignettes/*.Rmd`) of this repository.

## Workflow

To reproduce the analysis, obtain the data file from above-mentioned source. Then, run code contained in the following files

1. Data preparation
    - Code: `vignettes/prepare_data.Rmd`. 
    - Reads local file `leafnp_data_covariates_20210702.csv` (not in this repository).
    - Writes file `data/dfs_leafnp_20210729.rds`.
2. Feature elimination: The (computationally costly) job is performed in a High Performance Computing environment. 
    - Code: `src/submit_feature_elimination_leafnp.sh` and `analysis/feature_elimination_leafnp.R` run and evaluate jobs. 
    - Outputs from jobs are collected, analysed and visualised by `vignettes/feature_elimination_leafnp.Rmd`.
    - "Intermediate data files" created by the feature elimination and used in `vignettes/feature_elimination_leafnp.Rmd` are contained in this repository. 
    - Model coefficients from LMMs, shown Fig. 2 (d,e,f) are created in `vignettes/model_fitting.Rmd`.
3. Model fitting: 
    - Code: `vignettes/model_fitting.Rmd`
    - Outputs: 
        - **Fig. 3** in Tian et al. The figure file is contained in this repository (`fig/bars_model_fitting.pdf`). 
        - **Fig. 2** in Tian et al. The figure file is contained in this repository (`fig/bars_fe_tvals.pdf`). This requires `vignettes/feature_elimination_leafnp.Rmd` to be run first.
4. Trait gradient analysis: 
    - Code: `vignettes/traitgradient.Rmd`
    - Outputs: **Fig. 4** in Tian et al. The figure file is contained in this repository (`fig/tga_log_all.pdf`).
5. Demo of trait gradient analysis
    - Code: `vignettes/tga_hypotheses.qmd`
    - Outputs: `vignettes/tga_hypotheses.pdf`. This document is submitted as supplementary information to the article Tian et al. (in review).
6. Linear mixed effects model fitting explorations
    - Code: `vignettes/leafnp_fitting_order.Rmd`
    - Outputs: a html document showing results of the explorations. Results were used in the discussion of the article Tian et al. (in review).
    

## Repository structure

### Folder `analysis`

Contains scripts that can be ignored.

### Folder `data`

Contains data files produced by code of this repository.

### Folder `vignettes`

Contains implementations of analyses used for the study Tian et al. (in review).


## References

Tian, D., Kattge, J., Chen, Y., Han, W., Luo, Y., He, J., Hu, H., Tang, Z., Ma, S., Yan, Z., Lin, Q., Schmid, B., and Fang, J.: A global database of paired leaf nitrogen and phosphorus concentrations of terrestrial plants, Ecology, 100, e02812, https://doi.org/10.1002/ecy.2812, 2019.

Di Tian, Zhengbing Yan, Bernhard Schmid, Jens Kattge, Jingyun Fang, Benjamin D. Stocker: Environmental versus phylogenetic controls on leaf nitrogen and phosphorous concentrations of terrestrial plants. (in review)
