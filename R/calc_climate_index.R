## mean annual temperature
calc_climate_index_mat <- function(df, ...){
  df %>% 
    summarise(mat = mean(temp, ...))
}

## mean temperature during growing season
calc_climate_index_matgs <- function(df, temp_base = 5.0, ...){
  df %>% 
    dplyr::filter(temp >= temp_base) %>% 
    summarise(matgs = mean(temp, ...)) %>% 
    pull(matgs)
}

## Minimal monthly temperature (coldest month temperature)
calc_climate_index_tmonthmin <- function(df, ...){
  df %>% 
    mutate(month = lubridate::month(date)) %>% 
    group_by(month) %>% 
    summarise(temp = mean(temp, ...)) %>% 
    pull(temp) %>% 
    min()
}

## Maximal monthly temperature (warmest month temperature)
calc_climate_index_tmonthmax <- function(df, ...){
  df %>% 
    mutate(month = lubridate::month(date)) %>% 
    group_by(month) %>% 
    summarise(temp = mean(temp, ...)) %>% 
    pull(temp) %>% 
    max()
}

## Number of days with daily temperature above 0˚C (TMP0nb) or 5˚C (TMP5nb)
calc_climate_index_ndaysgs <- function(df, temp_base = 5.0, ...){
  df %>% 
    mutate(year = lubridate::year(date)) %>% 
    dplyr::filter(temp >= temp_base) %>% 
    group_by(year) %>% 
    summarise(ndays = n()) %>% 
    pull(ndays) %>% 
    mean()
}

## annual mean daily irradiance (PPFD)
calc_climate_index_mai <- function(df, ...){
  df %>% 
    summarise(mai = mean(ppfd, ...)) %>% 
    pull(mai)
}

## growing season mean daily irradiance
calc_climate_index_maigs <- function(df, temp_base = 5.0, ...){
  df %>% 
    dplyr::filter(temp >= temp_base) %>% 
    summarise(maigs = mean(ppfd, ...)) %>% 
    pull(maigs)
}

## mean annual summed precipitation
calc_climate_index_map <- function(df, ...){
  df %>% 
    mutate(prec = rain + snow) %>% 
    mutate(year = lubridate::year(date),
           prec = prec * (60*60*24)) %>% 
    group_by(year) %>% 
    summarise(map = sum(prec, ...)) %>% 
    pull(map) %>% 
    mean()
}

## Precipitation of Driest Month
calc_climate_index_pmonthmin <- function(df, ...){
  df %>% 
    mutate(prec = rain + snow) %>% 
    mutate(month = lubridate::month(date),
           year = lubridate::year(date),
           prec = prec * (60*60*24)) %>% 
    group_by(year, month) %>% 
    summarise(prec = sum(prec, ...)) %>% 
    ungroup() %>% 
    group_by(month) %>% 
    summarise(prec = mean(prec, ...)) %>% 
    pull(prec) %>% 
    min()
}

## mean growing season summed precipitation (mm)
calc_climate_index_mapgs <- function(df, temp_base = 5.0, ...){
  df %>% 
    mutate(prec = rain + snow) %>% 
    mutate(year = lubridate::year(date),
           prec = prec * (60*60*24)) %>% 
    dplyr::filter(temp >= temp_base) %>% 
    group_by(year) %>% 
    summarise(prec = sum(prec, ...)) %>% 
    pull(prec) %>% 
    mean()
}

## mean daytime VPD during the growing season
calc_climate_index_mavgs <- function(df, temp_base = 5.0, ...){
  df %>% 
    mutate(year = lubridate::year(date)) %>% 
    dplyr::filter(temp >= temp_base) %>% 
    group_by(year) %>% 
    summarise(vpd = mean(vpd, ...)) %>% 
    pull(vpd) %>% 
    mean()
}

## mean daytime VPD
calc_climate_index_mav <- function(df, ...){
  df %>% 
    summarise(vpd = mean(vpd, ...)) %>% 
    pull(vpd)
}

## potential evapotranspiration from SOFUN!