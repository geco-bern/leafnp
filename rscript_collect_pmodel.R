library(tidyverse)

open_pmodel <- function(ichunk){
  path <- paste0("data/df_pmodel_ichunk_", ichunk, "_100.RData")
  if (file.exists(path)){
    load(path)
  } else {
    df_pmodel <- tibble()
  }
  return(df_pmodel)
}

df <- purrr::map_dfr(as.list(seq(100)),
                     ~open_pmodel(.))

write_csv(df, file = "data/df_pmodel.csv")
