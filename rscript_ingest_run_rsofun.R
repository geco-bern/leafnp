#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
# args <- c(1,100)
print(args)

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
df_sites <- read_csv("~/leafnp/data/df_sites.csv") %>% 
  mutate(idx = 1:n())

## split sites data frame into (almost) equal chunks
nsites_per_chunk <- ceiling(nrow(df_sites)/args[2])
list_df_split <- split(df_sites, seq(nrow(df_sites)) %/% nsites_per_chunk)

# ## test
# df_test <- list_df_split %>% bind_rows()
# all_equal(df_test, df_sites)

## retain only the one required for this chunk
df_sites_sub <- list_df_split[[args[1]]]

print("This chunk contains these rows of the full site data frame:")
print(df_sites_sub$idx)

##------------------------------------------------------------------------
## ingest forcing data, run P-model, and get climate indeces at once
##------------------------------------------------------------------------
filn <- paste0("data/df_pmodel_ichunk_", as.character(args[1]), "_", as.character(args[2]), ".RData")
if (!file.exists(filn)){
  df_pmodel <- ingest_run_rsofun(df_sites_sub, ichunk = args[1], totchunk = args[2], verbose = TRUE)
  save(df_pmodel, file = filn)
} else {
  print(paste("File exists already: ", filn))
}

# ## memory profiling
# library(pryr)
# library(profvis)
# prof <- profvis({
#   df_pmodel <- ingest_run_rsofun(df_sites_sub, ichunk = args[1], totchunk = args[2])
# })
# 
# print(prof)

