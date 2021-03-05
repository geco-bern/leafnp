ingest_run_rsofun <- function(siteinfo){
  
  ddf_watch <- ingest(
    siteinfo = siteinfo,
    source    = "watch_wfdei",
    getvars   = c("temp", "prec", "ppfd", "vpd", "patm"),
    dir       = "~/data/watch_wfdei/",  # adjust this with your local path,
    settings  = list(correct_bias = "worldclim", dir_bias = "~/data/worldclim")
  )
  
  ddf_cru <- ingest(
    siteinfo = siteinfo,
    source    = "cru",
    getvars   = "ccov",
    dir       = "~/data/cru/ts_4.01/"  # adjust this with your local path
  )
  
  ddf_meteo <- ddf_watch %>% 
    tidyr::unnest(data) %>% 
    left_join(
      ddf_cru %>% 
        tidyr::unnest(data),
      by = c("sitename", "date")
    ) %>% 
    group_by(sitename) %>% 
    tidyr::nest()
  
  df_co2 <- ingestr::ingest(
    siteinfo,
    source  = "co2_mlo",
    verbose = FALSE,
    dir = "~/data/co2/"
  )
  
  ddf_fapar_unity <- ingest(
    siteinfo  = siteinfo,
    source    = "fapar_unity"
  )
  
  params_siml <- list(
    spinup             = TRUE,      # to bring soil moisture to steady state
    spinupyears        = 10,        # number of spinup years. 10 is enough for soil moisture.
    recycle            = 1,         # number of years recycled during spinup 
    soilmstress        = FALSE,     # boolean for whether soil moisture stress function is included
    tempstress         = FALSE,     # boolean for whether temperature stress function is included
    calc_aet_fapar_vpd = FALSE,     # set to FALSE - should be dropped again
    in_ppfd            = TRUE,      # if available from forcing files, set to TRUE
    in_netrad          = FALSE,     # if available from forcing files, set to TRUE
    outdt              = 1,
    ltre               = FALSE,
    ltne               = FALSE,
    ltrd               = FALSE,
    ltnd               = FALSE,
    lgr3               = TRUE,
    lgn3               = FALSE,
    lgr4               = FALSE
  )
  
  params_modl <- list(
    kphio           = 0.09423773,
    soilm_par_a     = 0.33349283,
    soilm_par_b     = 1.45602286
  )
  
  df_soiltexture <- bind_rows(
    top    = tibble(layer = "top",    fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1),
    bottom = tibble(layer = "bottom", fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1)
  )
  
  df_drivers <- collect_drivers_sofun( 
    siteinfo       = siteinfo,
    params_siml    = params_siml,
    meteo          = ddf_meteo, 
    fapar          = ddf_fapar_unity,
    co2            = df_co2,
    df_soiltexture = df_soiltexture
  )
  
  df_output <- runread_pmodel_f(
    df_drivers,
    params_modl = params_modl, 
    makecheck = TRUE,
    parallel = FALSE
  )
  
  ## gpp-weighted mean of vcmax25
  df_output <- df_output %>% 
    mutate(data = purrr::map(data, ~mutate(., vcmax25_wgt = vcmax25 * gpp))) %>% 
    mutate(data = purrr::map(data, ~summarise(., gpp_sum = sum(gpp), vcmax25_wgt_sum = sum(vcmax25_wgt)))) %>% 
    mutate(data = purrr::map(data, ~mutate(., vcmax25 = vcmax25_wgt_sum / gpp_sum))) %>% 
    unnest(data) %>% 
    dplyr::select(sitename, vcmax25)
  
  return(df_output)
  
}