ingest_run_rsofun <- function(siteinfo, ichunk = "X", totchunk = "XX", subchunk = "", verbose = FALSE){
  
  source("R/calc_climate_index.R")
  
  ## complement siteinfo with WHC based on S_CWDX80
  filn <- "~/data/mct_data/cwdx80.nc"
  siteinfo <- siteinfo %>% 
    left_join(rbeni::extract_pointdata_allsites(filn, df_lonlat = dplyr::select(siteinfo, sitename, lon, lat)) %>% 
                setNames(c("whc", "lon", "lat")),
              by = c("lon", "lat")
              )
  
  ## fill gaps in whc
  whc_median <- median(siteinfo$whc, na.rm = TRUE)
  siteinfo <- siteinfo %>% 
    mutate(whc = ifelse(is.na(whc), whc_median, whc))
  
  path_watch <- paste0("data/ddf_watch_chunk", as.character(subchunk), "_", as.character(ichunk), "_", as.character(totchunk), ".RData")
  if (!file.exists(path_watch)){
    
    out_mem <- pryr::mem_change(
      ddf_watch <- ingest(
        siteinfo = siteinfo,
        source    = "watch_wfdei",
        getvars   = c("temp", "prec", "ppfd", "vpd", "patm"),
        dir       = "~/data/watch_wfdei/",  # adjust this with your local path,
        settings  = list(correct_bias = "worldclim", dir_bias = "~/data/worldclim")
      )
    )
    save(ddf_watch, file = path_watch)    
    if (verbose) print("Memory change 1:")
    if (verbose) (out_mem)
    
  } else {
    
    load(path_watch)
    
    ## check if any precip data is nan. if so, run ingestr again after it has been bugfixed.
    problem_prec <- ddf_watch %>% 
      unnest(data) %>% 
      pull(prec) %>% 
      is.nan() %>% 
      any()
    problem_ppfd <- ddf_watch %>% 
      unnest(data) %>% 
      pull(ppfd) %>% 
      is.nan() %>% 
      any()
    problem_vpd <- ddf_watch %>% 
      unnest(data) %>% 
      pull(vpd) %>% 
      is.nan() %>% 
      any()
    
    if (problem_prec || problem_vpd || problem_ppfd){
      ddf_watch <- ingest(
        siteinfo = siteinfo,
        source    = "watch_wfdei",
        getvars   = c("temp", "prec", "ppfd", "vpd", "patm"),
        dir       = "~/data/watch_wfdei/",  # adjust this with your local path,
        settings  = list(correct_bias = "worldclim", dir_bias = "~/data/worldclim")
      )
      save(ddf_watch, file = path_watch)  
    }
    
  }
  
  path_cru <- paste0("data/ddf_cru_chunk", as.character(subchunk), "_", as.character(ichunk), "_", as.character(totchunk), ".RData")
  if (!file.exists(path_cru)){
    
    out_mem <- pryr::mem_change(
      ddf_cru <- ingest(
        siteinfo = siteinfo,
        source    = "cru",
        getvars   = "ccov",
        dir       = "~/data/cru/ts_4.01/"  # adjust this with your local path
      )
    )
    save(ddf_cru, file = path_cru)
    if (verbose) print("Memory change 2:")
    if (verbose) print(out_mem)
    
  } else {
    load(path_cru)
  }
  
  ddf_meteo <- ddf_watch %>% 
    tidyr::unnest(data) %>% 
    left_join(
      ddf_cru %>% 
        tidyr::unnest(data),
      by = c("sitename", "date")
    ) %>% 
    group_by(sitename) %>% 
    tidyr::nest()
  
  df_co2 <- ingest(
    siteinfo,
    source  = "co2_cmip",
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
  
  ## run P-model for each site
  df_output <- runread_pmodel_f(
    df_drivers,
    params_modl = params_modl, 
    makecheck = TRUE,
    parallel = FALSE
  )
  
  ## get mean annual AET separately
  df_aet <- df_output %>% 
    unnest(data) %>% 
    mutate(year = lubridate::year(date)) %>% 
    group_by(sitename, year) %>% 
    summarise(transp = sum(transp)) %>% 
    ungroup() %>% 
    group_by(sitename) %>% 
    summarise(aet = mean(transp))
  
  df_output <- df_output %>% 
    
    ## calculate mean over daily AET/PET
    mutate(data = purrr::map(data, ~mutate(., alpha = transp / pet))) %>% 
    
    ## gpp-weighted mean of vcmax25, jmax25, gs_accl, mean daily AET/PET
    mutate(data = purrr::map(data, ~mutate(., vcmax25_wgt = vcmax25 * gpp, jmax25_wgt = jmax25 * gpp, gs_accl_wgt = gs_accl * gpp))) %>% 
    mutate(data = purrr::map(data, ~summarise(., 
                                              gpp_sum = sum(gpp), 
                                              vcmax25_wgt_sum = sum(vcmax25_wgt), 
                                              jmax25_wgt_sum = sum(jmax25_wgt), 
                                              gs_accl_wgt_sum = sum(gs_accl_wgt),
                                              alpha = mean(alpha)))) %>% 
    mutate(data = purrr::map(data, ~mutate(., vcmax25 = vcmax25_wgt_sum / gpp_sum, jmax25 = jmax25_wgt_sum / gpp_sum, gs_accl = gs_accl_wgt_sum / gpp_sum))) %>% 
    unnest(data) %>% 
    dplyr::select(sitename, alpha, vcmax25, jmax25, gs_accl)
  
  ## get climate indeces from ddf_watch
  df_output <- ddf_watch %>% 
    mutate(mat = purrr::map_dbl(data, ~calc_climate_index_mat(.)),
           matgs = purrr::map_dbl(data, ~calc_climate_index_matgs(., temp_base = 5.0)),
           tmonthmin = purrr::map_dbl(data, ~calc_climate_index_tmonthmin(.)),
           tmonthmax = purrr::map_dbl(data, ~calc_climate_index_tmonthmax(.)),
           ndaysgs = purrr::map_dbl(data, ~calc_climate_index_ndaysgs(., temp_base = 5.0)),
           mai = purrr::map_dbl(data, ~calc_climate_index_mai(.)),
           maigs = purrr::map_dbl(data, ~calc_climate_index_maigs(., temp_base = 5.0)),
           map = purrr::map_dbl(data, ~calc_climate_index_map(.)),
           pmonthmin = purrr::map_dbl(data, ~calc_climate_index_pmonthmin(.)),
           mapgs = purrr::map_dbl(data, ~calc_climate_index_mapgs(., temp_base = 5.0)),
           mavgs = purrr::map_dbl(data, ~calc_climate_index_mavgs(., temp_base = 5.0)),
           mav = purrr::map_dbl(data, ~calc_climate_index_mav(.))) %>% 
    dplyr::select(-data) %>% 
    right_join(df_output, by = "sitename") %>% 
    left_join(df_aet, by = "sitename") %>% 
    mutate(ai = map / aet)
  
  ## add S_CWDX80 to output as an additional climate index
  df_output <- df_output %>% 
    left_join(dplyr::select(siteinfo, sitename, cwdx80 = whc), by = "sitename")
  
  return(df_output)
  
}