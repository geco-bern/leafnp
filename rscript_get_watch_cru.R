library(ingestr)
library(tidyverse)

ddf_watch <- ingest(
  siteinfo = df_cells,
  source    = "watch_wfdei",
  getvars   = c("temp", "prec", "ppfd", "vpd", "patm"),
  dir       = "~/data/watch_wfdei/"  # adjust this with your local path
)
save(ddf_watch, file = "data/ddf_watch.RData")


ddf_cru <- ingest(
  siteinfo = df_cells,
  source    = "cru",
  getvars   = "ccov",
  dir       = "~/data/cru/ts_4.01/"  # adjust this with your local path
)
save(ddf_cru, file = "data/ddf_cru.RData")
