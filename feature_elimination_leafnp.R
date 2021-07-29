#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(tidyverse)
library(ranger)
library(caret)
library(recipes)
library(readr)

dfs <- read_rds("data/dfs_leafnp.rds")

## specify target variable (as above)
# target <- 'LeafNP'
target <- as.character(args[1])

## predictors excluding PHO, and TS (too many missing)
preds <- c("elv", "mat", "matgs", "tmonthmin", "tmonthmax", "ndaysgs", "mai", "maigs", "map", "pmonthmin",
           "mapgs", "mavgs", "mav", "alpha", "vcmax25", "jmax25", "gs_accl", "aet", "ai", "cwdx80", "gti",
           "ndep", "co2", "T_BULK_DENSITY", "AWC_CLASS", "T_CLAY", "T_SILT", "T_SAND", "T_GRAVEL", "T_PH_H2O",
           "T_TEB", "T_BS", "T_CEC_SOIL", "T_CEC_CLAY", "T_ECE", "T_ESP", "T_CACO3", "T_OC", "ORGC", "TOTN",
           "CNrt", "ALSA", "PBR", "TP", "TK")

# preds <- c("elv", "cwdx80", "ndep", "co2")

# This is the vector of candidate predictors to be added in the model. To begin with, consider all as candidates.
preds_candidate <- preds 

# predictors retained in the model from the previous step. To begin with, is all predictors
preds_retained <- preds

## work with lists as much as possible (more flexible!)
df_fe_summary <- tibble()

## common train control params
traincotrlParams <- trainControl( 
  method="cv", 
  number=5, 
  verboseIter=FALSE,
  savePredictions = "final"
)

## for output
df_fe <- tibble()

## outer loop for k predictors
for (k_index in 1:(length(preds)-1)){

  ## for determining variable to drop
  df_fe_candidates <- tibble()
  # fit_candidates <- list()
  
  ## inner loop for single additional predictor
  for (ipred in preds_candidate){
    
    # variable vector (new variable + retained variables) used in regression
    preds_after_drop <- preds_retained[-which(preds_retained == ipred)]
    
    # define formulate with newly-added predictor
    forml  <- as.formula(paste( target, '~', paste(preds_after_drop, collapse = '+')))
    
    # fit random forest model
    ## create generic formula for the model and define preprocessing steps
    pp <- recipe(forml, data = dplyr::select(dfs, target, preds_after_drop)) %>%
      step_impute_median(all_predictors())
      # step_medianimpute(all_predictors())   # for old version
    
    ## No actual tuning here
    tune_grid <- expand.grid( .mtry = floor((length(preds_after_drop)-1) / 3), 
                              .min.node.size = 5,
                              .splitrule = "variance")
    set.seed(1982)
    
    fit <- train(
      pp,
      data            = dplyr::select(dfs, target, all_of(preds_after_drop)),
      metric          = "RMSE",
      method          = "ranger",
      tuneGrid        = tune_grid,
      trControl       = traincotrlParams,
      replace         = FALSE,
      sample.fraction = 0.5
      # num.trees       = 2000,        # boosted for the final model
      # importance      = "impurity"   # for variable importance analysis, alternative: "permutation"
    )
    
    # # add model object to list, and name the element according to the added variable
    # fit_candidates[[ k_index ]][[ ipred ]] <- fit
    
    # record metrics for all candidates
    rsq <- fit$results$Rsquared
    df_fe_candidates <- bind_rows(df_fe_candidates, tibble(level = k_index, pred = ipred, rsq = rsq))
    
    print(paste("R2 = ", rsq, " after dropping", ipred))
    
  }
  
  ## record for output
  df_fe <- df_fe %>% 
    bind_rows(df_fe_candidates)
  
  ## get name of candidate predictor for which, when dropped, the model still achieved the highest R2.
  pred_drop <- df_fe_candidates %>%  # when storing R2 in a data frame
    arrange(desc(rsq)) %>% 
    slice(1) %>% 
    pull(pred) %>% 
    as.character()
  
  rsq_new <- df_fe_candidates %>%  # when storing R2 in a data frame
    arrange(desc(rsq)) %>% 
    slice(1) %>% 
    pull(rsq) %>% 
    as.numeric()
  
  print("*********************")
  print(paste("Dropping", pred_drop, " R2 = ", rsq_new))
  print("*********************")
  
  ## exit feature elimination once R2 drops below 0.45
  if (target == "leafN"){
    if (rsq_new < 0.45) break
  } else if (target == "leafP"){
    if (rsq_new < 0.30) break
  } else if (target == "LeafNP"){
    if (rsq_new < 0.17) break
  }
  
  ## drop next unnecessary predictor  
  preds_retained <- preds_retained[-which(preds_retained == pred_drop)]
  
  # record CV r2 of respective model
  df_fe_summary <- df_fe_summary %>% 
    bind_rows(
      tibble( pred = pred_drop,
                  rsq = rsq_new
      )
    )
  
  # remove the selected variable from the candidate variable list
  preds_candidate <- preds_candidate[-which(preds_candidate == pred_drop)]
  
}

## add result from before dropping
set.seed(1982)

# define formulate with newly-added predictor
forml  <- as.formula(paste( target, '~', paste(preds, collapse = '+')))

# fit random forest model
pp <- recipe(forml, data = dplyr::select(dfs, target, preds)) %>%
  step_impute_median(all_predictors())
  # step_medianimpute(all_predictors())   # for old version

fit <- train(
  pp,
  data            = dplyr::select(dfs, target, all_of(preds)),
  metric          = "RMSE",
  method          = "ranger",
  tuneGrid        = tune_grid,
  trControl       = traincotrlParams,
  replace         = FALSE,
  sample.fraction = 0.5)

# # add model object to list, and name the element according to the added variable
# fit_candidates[[ 0 ]][[ ipred ]] <- fit

# record metrics for all candidates
rsq <- fit$results$Rsquared
df_fe_candidates <- tibble(level = 0, pred = NA, rsq = rsq)

print(paste("R2 = ", rsq, " with all predictors included"))
print("*********************")

## record for output
df_fe <- df_fe %>% 
  bind_rows(df_fe_candidates)

# record CV r2 of respective model
df_fe_summary <- df_fe_summary %>% 
  bind_rows(
    tibble( pred = NA,
            rsq = rsq
    )
  )

## nicely readable variable importance
df_vip <- df_fe %>% 
  mutate(vip = dplyr::filter(df_fe, is.na(pred)) %>% pull(rsq) - rsq) %>% 
  dplyr::filter(level == 1) %>% 
  mutate(pred = fct_reorder(pred, vip))
  
write_csv(df_fe_summary, file = "data/df_fe_summary.csv")
write_csv(df_fe, file = "data/df_fe.csv")
write_csv(df_vip, file = "data/df_vip.csv")

## plot variable importance determined at level 1 and save as file
df_vip %>% 
  ggplot(aes(pred, vip)) +
  geom_bar(stat = "identity") +
  coord_flip()

ggsave("fig/vip_fe.pdf")

# df <- read_csv("rankvars_fe_leafnp.csv") %>% 
#   mutate(order = rev(1:n())) %>% 
#   mutate(variable = fct_reorder(variable, order))
# 
# df %>% 
#   ggplot(aes(variable, rsq)) +
#   geom_bar(stat = "identity") +
#   coord_flip()
# 
# df %>% 
#   dplyr::filter(rsq == max(rsq))
# 
# ## determine important set of parameters based on visual inspection: 
# ## drop all until rsq starts declining
# vars_not_important <- pull(df, variable) %>% as.vector()
# vars_not_important <- vars_not_important[!(vars_not_important %in% c("elv", "mav", "ALSA", "ndaysgs"))]
# preds[!(preds %in% vars_not_important)]
  
