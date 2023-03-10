library(dplyr)

check_avail_pmodel <- function(ichunk){

  ## written by rscript_get_data_alexi.R -> rbeni::nclist_to_df()
  dir <- "data/"
  filnam <- paste0("df_pmodel_ichunk_", ichunk, "_100.RData")
  avl <- file.exists(paste0(dir, filnam))

  return(avl)
}

df <- tibble(ichunk = 1:100) %>%
  rowwise() %>%
  mutate(
    avl_pmodel = check_avail_pmodel(ichunk)
  )

save(df, file = "./data/df_file_availability.RData")

df %>% dplyr::filter(!avl_pmodel)

## problem with 30, row 10, sitename i_109.1628_19.6431_20
