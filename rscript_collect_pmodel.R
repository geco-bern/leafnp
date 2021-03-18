library(tidyverse)

open_pmodel <- function(ichunk){
  path <- paste0("data/df_pmodel_ichunk_", ichunk, "_100.RData")
  if (file.exists(path)){
    load(path)
    return(df_pmodel)
  } else {
    return(tibble())
  }
}

df <- purrr::map_dfr(as.list(seq(100)),
                     ~open_pmodel(.))

write_csv(df, file = "data/df_pmodel_leafnp.csv")
