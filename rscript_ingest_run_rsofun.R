#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
#args <- c(1,1000)

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(rlang)
library(lubridate)
library(ingestr)
library(rsofun)
library(rbeni)

source("R/ingest_run_rsofun.R")

## read sites data frame
df_sites <- read_csv("data/df_sites.csv")

##------------------------------------------------------------------------
## split sites up to this chunk
##------------------------------------------------------------------------
nsites <- nrow(df_sites)
vec_isites <- seq(nsites)
nchunk <- as.integer(args[2]) # 1000  # make sure this is consistent with the number of parallel jobs (job array!) in the submission script
nrows_chunk <- ceiling(nsites/nchunk)
list_irow_chunk <- split(vec_isites, ceiling(seq_along(vec_isites)/nrows_chunk))
irow_chunk <- list_irow_chunk[[as.integer(args[1])]]

print("getting data for longitude indices:")
print(irow_chunk) 

df_sites_sub <- df_sites %>% 
  slice(irow_chunk)

##------------------------------------------------------------------------
## ingest forcing data, run P-model, and get climate indeces at once
##------------------------------------------------------------------------
df_pmodel <- ingest_run_rsofun(df_sites_sub, ichunk = args[1], totchunk = args[2])

save(df_pmodel, file = paste0("data/df_pmodel_ichunk_", as.character(args[1]), "_", as.character(args[2]), ".RData"))