#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
#args <- c(1,100)

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(multidplyr)
library(rlang)
library(lubridate)
library(ingestr)
library(rsofun)

source("R/ingest_run_rsofun.R")

## read sites data frame
df_sites <- read_csv("data/df_sites.csv")

##------------------------------------------------------------------------
## split it up into chunks (total number of chunks provided by argument 2)
##------------------------------------------------------------------------
nsites <- nrow(df_sites)
vec_isites <- seq(nsites)
nchunk <- as.integer(args[2]) # 1000  # make sure this is consistent with the number of parallel jobs (job array!) in the submission script
nrows_chunk <- ceiling(nsites/nchunk)
list_irow_chunk <- split(vec_isites, ceiling(seq_along(vec_isites)/nrows_chunk))
irow_chunk <- list_irow_chunk[[as.integer(args[1])]]
df_sites_sub <- df_sites %>% slice(irow_chunk)

print("getting data for longitude indices:")
print(irow_chunk) 

## get all available cores
ncores <- parallel::detectCores()

## limit the number of cores to number of individual runs
nruns <- length(irow_chunk)
ncores <- min(ncores, nruns)

if (ncores > 1){

  cl <- multidplyr::new_cluster(ncores) %>%
    multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr", "lubridate", "rlang", "ingestr", "rsofun")) %>%
    multidplyr::cluster_assign(ingest_run_rsofun = ingest_run_rsofun)

  ## distribute to cores, making sure all data from a specific site is sent to the same core
  df_out <- tibble(ilon = irow_chunk) %>%
    multidplyr::partition(cl) %>%
    dplyr::mutate(out = purrr::map( ilon,
                                    ~ingest_run_rsofun(.)))

} else {

  ## testing
  df_out <- purrr::map(as.list(irow_chunk), ~ingest_run_rsofun(.))

}