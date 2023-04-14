## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(purrr)
library(rbeni)
library(ingestr)
library(visdat)


## -------------------------------------------------------------------------------------------------------------------------------------
## original data, no covariates
df <- read_csv("~/data/LeafNP_tiandi/Global_total_leaf_N_P_Di/global_leaf_NP_total_Di_20210702_PROC_ELV.csv")

## sites (unique lon, lat, elv)
df_sites <- read_csv("~/data/LeafNP_tiandi/Global_total_leaf_N_P_Di/df_sites_leafnp_20210702.csv")

## covariates added, but incomplete
df_cov <- read_csv("~/data/LeafNP_tiandi/Global_total_leaf_N_P_Di/update_data_2021July/updated_dataset_20210429.csv")


## -------------------------------------------------------------------------------------------------------------------------------------
df_cov %>% vis_miss(warn_large_data = FALSE)


## -------------------------------------------------------------------------------------------------------------------------------------
df_cov %>% 
  summarise(across(where(is.numeric), ~(sum(is.na(.x)) / length(.x)))) %>% 
  pivot_longer(cols = 1:ncol(.), names_to = "name", values_to = "value") %>% 
  dplyr::filter(value > 0)


## ----eval=FALSE-----------------------------------------------------------------------------------------------------------------------
## filn <- "data/df_hwsd_20210324.RData"
## load(filn)


## -------------------------------------------------------------------------------------------------------------------------------------
## small number of data is missing
vis_miss(df_hwsd)


## -------------------------------------------------------------------------------------------------------------------------------------
sites_missing_hwsd <- df_hwsd %>% 
  dplyr::filter(is.na(T_BULK_DENSITY)) %>%  #  | is.na(T_GRAVEL) | is.na(AWC_CLASS)
  pull(sitename) %>% 
  unique()

df_sites_missing <- df_sites %>% 
  dplyr::filter(sitename %in% sites_missing_hwsd)

plot_map_simpl() +
  geom_point(data = df_sites_missing, aes(x = lon, y = lat), color = "red", size = 0.3)


## -------------------------------------------------------------------------------------------------------------------------------------
df_hwsd_compl <- ingest(
  df_sites_missing,
  source = "hwsd",
  settings = list(fil = "~/data/hwsd/HWSD_RASTER/hwsd.bil")
  )

df_hwsd_compl %>% 
  unnest(data) %>% 
  vis_miss()


## -------------------------------------------------------------------------------------------------------------------------------------
load("data/df_wise_20210324.RData")


## -------------------------------------------------------------------------------------------------------------------------------------
vis_miss(df_wise)


## -------------------------------------------------------------------------------------------------------------------------------------
load("data/df_gsde_20210702.RData")


## -------------------------------------------------------------------------------------------------------------------------------------
vis_miss(df_gsde)


## -------------------------------------------------------------------------------------------------------------------------------------
sites_missing_gsde <- df_gsde %>% 
  dplyr::filter(is.na(PBR)) %>%  #  | is.na(T_GRAVEL) | is.na(AWC_CLASS)
  pull(sitename) %>% 
  unique()

df_sites_missing <- df_sites %>% 
  dplyr::filter(sitename %in% sites_missing_gsde)

plot_map_simpl() +
  geom_point(data = df_sites_missing, aes(x = lon, y = lat), color = "red", size = 0.3)


## -------------------------------------------------------------------------------------------------------------------------------------
settings_gsde <- list(varnam =  c("PBR"), layer = 1:4)    # , "PHO", "TP", "TK", "TS"

df_gsde_compl <- ingest(
  df_sites_missing %>% slice(1:3),
  source    = "gsde",
  settings  = settings_gsde,
  dir       = "~/data/soil/shangguan"
  ) %>% 
  unnest(data)


## -------------------------------------------------------------------------------------------------------------------------------------
load("data/df_gti_20210324.RData")
vis_miss(df_gti)


## -------------------------------------------------------------------------------------------------------------------------------------
load("data/df_co2_20210324.RData")
vis_miss(df_co2)


## -------------------------------------------------------------------------------------------------------------------------------------
load("data/df_ndep_20210324.RData")
vis_miss(df_ndep_agg)


## -------------------------------------------------------------------------------------------------------------------------------------
read_pmodel <- function(path){
  load(path)
  return(df_pmodel)
}
df_pmodel <- purrr::map_dfr(as.list(list.files("data", pattern = "df_pmodel_ichunk_.*.RData", full.names = TRUE)),
                            ~read_pmodel(.))

save(df_pmodel, file = "data/df_pmodel.RData")

vis_miss(df_pmodel)


## -------------------------------------------------------------------------------------------------------------------------------------
load("data/df_ndep_20210324.RData") # loads df_ndep_agg
load("data/df_co2_20210324.RData") # df_co2
load("data/df_gti_20210324.RData") # loads df_gti
load("data/df_gsde_20210702.RData") # df_gsde
load("data/df_wise_20210324.RData") # df_wise
load("data/df_hwsd_20210324.RData") # df_hwsd

df <- read_csv("~/data/LeafNP_tiandi/Global_total_leaf_N_P_Di/global_leaf_NP_total_Di_20210702_PROC_ELV.csv")

df_my <- df %>% 
  dplyr::select(-Record_ID, -soil_N, -soil_P) %>% 
  left_join(df_pmodel, by = "sitename") %>% 
  left_join(df_gti %>% dplyr::select(-year_start, -year_end, -elv, -lon, -lat), by = "sitename") %>% 
  left_join(df_ndep_agg, by = "sitename") %>% 
  left_join(df_co2, by = "sitename") %>% 
  left_join(df_hwsd, by = "sitename") %>% 
  left_join(df_wise, by = "sitename") %>% 
  left_join(df_gsde, by = "sitename")

vis_miss(df_my, warn_large_data = F)
vis_miss(df_my %>% dplyr::select(58:68), warn_large_data = F)

ggsave("fig/missing.pdf", width = 12, height = 6)


## -------------------------------------------------------------------------------------------------------------------------------------
write_csv(df_my, file = "~/data/LeafNP_tiandi/Global_total_leaf_N_P_Di/leafnp_data_covariates_20210702.csv")

