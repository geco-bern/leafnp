## -------------------------------------------------------------------------------------
#knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(ingestr)
library(rsofun)
source("R/ingest_run_rsofun.R")


## -------------------------------------------------------------------------------------
df_sites <- tibble(
  sitename = c("site_rsofun_demo_1", "site_rsofun_demo_2", "site_rsofun_demo_3"),
  lon = c(100.05, 100.45, 90),
  lat = c(50, 50, 50),
  elv = c(1000, 250, 2500),
  year_start = c(1960, 2001, 2005),
  year_end = c(2005, 2006, 2007),
  whc = 200
)

df <- ingest_run_rsofun(df_sites)


## -------------------------------------------------------------------------------------
df_sites <- read_csv("data/df_sites.csv")


## -------------------------------------------------------------------------------------
## bin
dlon <- 0.5
dlat <- 0.5
lon_breaks <- seq(from = floor(min(df_sites$lon)), to = ceiling(max(df_sites$lon)), by = dlon)
lat_breaks <- seq(from = floor(min(df_sites$lat)), to = ceiling(max(df_sites$lat)), by = dlat)

df_sites <- df_sites %>%
  ungroup() %>% 
  mutate(ilon = cut(lon, 
                    breaks = lon_breaks
                    ),
         ilat = cut(lat, 
                    breaks = lat_breaks
                    )
         ) %>% 
  mutate(lon_lower = as.numeric( sub("\\((.+),.*", "\\1", ilon)),
         lon_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ilon) ),
         lat_lower = as.numeric( sub("\\((.+),.*", "\\1", ilat) ),
         lat_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ilat) )
         ) %>% 
  mutate(lon_mid = (lon_lower + lon_upper)/2,
         lat_mid = (lat_lower + lat_upper)/2) %>% 
  dplyr::select(-ilon, -ilat, -lon_lower, -lon_upper, -lat_lower, -lat_upper) %>% 
  mutate(cellname = paste0("icell_", lon_mid, "_", lat_mid))

## save again, now with lon_mid, lat_mid
save(df_sites, file = "data/df_sites.RData")

df_cells <- df_sites %>% 
  dplyr::select(sitename = cellname, lon = lon_mid, lat = lat_mid) %>% 
  distinct() %>% 

  ## set for reading all available years, subset to required years for each site later
  mutate(year_start = 1979, year_end = 2018)

write_csv(df_cells, path = "data/df_cells.csv")


## ----eval=FALSE-----------------------------------------------------------------------
## df_cells <- read_csv("data/df_cells.csv")
## 
## ddf_watch <- ingest(
##   siteinfo = df_cells,
##   source    = "watch_wfdei",
##   getvars   = c("temp", "prec", "ppfd", "vpd"),
##   dir       = "~/data/watch_wfdei/"  # adjust this with your local path
## )
## save(ddf_watch, file = "data/ddf_watch.RData")
## 
## ## make flat and write to file
## ddf_watch %>%
##   unnest(data) %>%
##   write_csv(file =  "data/ddf_watch.csv")
## 
## ddf_cru <- ingest(
##   siteinfo = df_cells,
##   source    = "cru",
##   getvars   = "ccov",
##   dir       = "~/data/cru/ts_4.01/"  # adjust this with your local path
## )
## save(ddf_cru, file = "data/ddf_cru.RData")
## 
## ## make flat and write to file
## ddf_cru %>%
##   unnest(data) %>%
##   write_csv(file =  "data/ddf_cru.csv")


## ----eval=FALSE-----------------------------------------------------------------------
## load("data/ddf_watch.RData")
## load("data/ddf_cru.RData")
## 
## ddf_meteo <- ddf_watch %>%
##   tidyr::unnest(data) %>%
##   left_join(
##     ddf_cru %>%
##       tidyr::unnest(data),
##     by = c("sitename", "date")
##   ) %>%
##   group_by(sitename) %>%
##   tidyr::nest()


## ----eval=FALSE-----------------------------------------------------------------------
## df_co2 <- read_csv("~/data/co2/cCO2_rcp85_const850-1765.csv")


## -------------------------------------------------------------------------------------
params_siml <- list(
  spinup             = TRUE,
  spinupyears        = 10,
  recycle            = 1,
  soilmstress        = TRUE,
  tempstress         = TRUE,
  calc_aet_fapar_vpd = FALSE,
  in_ppfd            = TRUE,
  in_netrad          = FALSE,
  outdt              = 1,
  ltre               = FALSE,
  ltne               = FALSE,
  ltrd               = FALSE,
  ltnd               = FALSE,
  lgr3               = TRUE,
  lgn3               = FALSE,
  lgr4               = FALSE
	)

## calibrated parameters for v3.0 (see https://rpubs.com/stineb/rsofun_benchmark_v30)
params_modl <- list(
	kphio           = 0.09423773,
	soilm_par_a     = 0.33349283,
	soilm_par_b     = 1.45602286,
	vpdstress_par_a = 9999,
	vpdstress_par_b = 9999,
	vpdstress_par_m = 9999
	)

df_soiltexture <- bind_rows(
  top    = tibble(layer = "top",    fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1),
  bottom = tibble(layer = "bottom", fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1)
)


## -------------------------------------------------------------------------------------
df_drivers <- prepare_setup_sofun(siteinfo = df_sites, params_siml = params_siml)


## -------------------------------------------------------------------------------------
df_drivers <- df_drivers %>% 
  dplyr::select(sitename, lon, lat, elv) %>% 
  mutate(c4 = FALSE, whc = 240) %>% 
  group_by(sitename) %>% 
  nest() %>% 
  rename(siteinfo = data) %>% 
  left_join(df_drivers %>% 
              dplyr::select(sitename, params_siml, cellname),
            by = "sitename")


## -------------------------------------------------------------------------------------
df_co2 <- read_csv("~/data/co2/cCO2_rcp85_const850-1765.csv")

subset_forcing <- function(df_forcing, params_siml){
  
  ## get mean seasonality in forcing to be used for all years before 1979
  df_forcing_meandoy <- df_forcing %>% 
    # dplyr::filter(year(date) %in% 1979:1988) %>% 
    ungroup() %>% 
    mutate(doy = lubridate::yday(date)) %>% 
    group_by(doy) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE)
  
  ## initialise time series for required years
  ddf <- init_dates_dataframe(
    yrstart = params_siml$firstyeartrend[1],
    yrend = params_siml$firstyeartrend[1] + params_siml$nyeartrend[1] - 1,
    noleap = TRUE
    )

  ## add available forcing data to given time series 
  ddf <- ddf %>% 
    left_join(df_forcing, by = "date") %>% 
    
    ## add mean seasonal cycle
    mutate(doy = lubridate::yday(date)) %>% 
    left_join(df_forcing_meandoy %>% 
                rename(temp_doy = temp, patm_doy = patm, qair_doy = qair, vpd_doy = vpd,
                       ppfd_doy = ppfd, prec_doy = prec, ccov_doy = ccov),
                by = "doy") %>% 
    
    ## fill missing with mean seasonal cycle
    rowwise() %>% 
    mutate(temp = ifelse(is.na(temp), temp_doy, temp),
           vpd  = ifelse(is.na(vpd),  vpd_doy,  vpd),
           patm = ifelse(is.na(patm), patm_doy, patm),
           ppfd = ifelse(is.na(ppfd), ppfd_doy, ppfd),
           prec = ifelse(is.na(prec), prec_doy, prec),
           ccov = ifelse(is.na(ccov), ccov_doy, ccov)
    ) %>% 
    
    ## merge co2 data in it
    mutate(year = lubridate::year(date)) %>% 
    left_join(df_co2, by = "year") %>% 
    
    ## remove days in leap years
    dplyr::filter(!(month(date)==2 & mday(date) == 29))
  
  return(ddf)
}

df_drivers <- df_drivers %>% 
  left_join(ddf_meteo %>% 
              rename(cellname = sitename), 
            by = "cellname") %>% 
  mutate(forcing = purrr::map2(forcing, params_siml, ~subset_forcing(.x, .y)))


## -------------------------------------------------------------------------------------
df_drivers <- df_soiltexture %>%
  dplyr::mutate(tmp = 1) %>% 
  group_by(tmp) %>% 
  nest() %>% 
  rename(df_soiltexture = data) %>% 
  right_join(
    df_drivers %>% 
      mutate(tmp = 1),
    by = "tmp"
  ) %>% 
  dplyr::select(-tmp)


## -------------------------------------------------------------------------------------
df_drivers <- df_drivers %>% 
  dplyr::select(sitename, params_siml, siteinfo, df_soiltexture, forcing)


## -------------------------------------------------------------------------------------
df_pmodel <- runread_pmodel_f(
  df_drivers,
  params_modl = params_modl,
  makecheck = TRUE,
  parallel = FALSE
  )

save(df_pmodel, file = "data/df_pmodel.RData")

df_pmodel_flat <- df_pmodel %>% 
  unnest(out)
write_csv(df_pmodel_flat, path = "data/df_pmodel_flat.csv")

