library(tidyverse)

open_pmodel <- function(ichunk){
  path <- paste0("data/df_pmodel_ichunk_", ichunk, "_100.RData")
  load(path)
  return(df_pmodel)
}

df <- purrr::map_dfr(as.list(seq(3)),
                     ~open_pmodel(.))

write_csv(df, file = "data/df_pmodel.csv")