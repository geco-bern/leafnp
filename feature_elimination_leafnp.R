library(tidyverse)
library(ranger)
library(caret)
library(visdat)
library(vip)
library(pdp)
library(nnet)
library(recipes)

dfs <- read_rds("data/dfs_leafnp.rds")

## predictors excluding PHO, and TS (too many missing)
preds <- c("elv", "mat", "matgs", "tmonthmin", "tmonthmax", "ndaysgs", "mai", "maigs", "map", "pmonthmin", 
           "mapgs", "mavgs", "mav", "alpha", "vcmax25", "jmax25", "gs_accl", "aet", "ai", "cwdx80", "gti", 
           "ndep", "co2", "T_BULK_DENSITY", "AWC_CLASS", "T_CLAY", "T_SILT", "T_SAND", "T_GRAVEL", "T_PH_H2O", 
           "T_TEB", "T_BS", "T_CEC_SOIL", "T_CEC_CLAY", "T_ECE", "T_ESP", "T_CACO3", "T_OC", "ORGC", "TOTN", 
           "CNrt", "ALSA", "PBR", "TP", "TK")

## specify target variable (as above)
target <- 'leafN'

# This is the vector of candidate predictors to be added in the model. To begin with, consider all as candidates.
preds_candidate <- preds 

# predictors retained in the model from the previous step. To begin with, is all predictors
preds_retained <- preds

## work with lists as much as possible (more flexible!)
df_metrics <- data.frame()

## common train control params
traincotrlParams <- trainControl( 
  method="cv", 
  number=5, 
  verboseIter=FALSE,
  savePredictions = "final"
)

## outer loop for k predictors
for (k_index in 1:length(preds)){
  
  # rsq_candidates <- c()
  df_rsq_candidates <- data.frame()
  fit_candidates <- list()
  
  ## inner loop for single additional predictor
  for (ipred in preds_candidate){
    
    # variable vector (new variable + retained variables) used in regression
    preds_after_drop <- preds_retained[-which(preds_retained == ipred)]
    
    # define formulate with newly-added predictor
    forml  <- as.formula(paste( target, '~', paste(preds_after_drop, collapse = '+')))
    
    # fit random forest model
    ## create generic formula for the model and define preprocessing steps
    pp <- recipe(forml, data = dplyr::select(dfs, leafN, preds)) %>%
      step_impute_median(all_predictors())
    
    ## No actual tuning here
    tune_grid <- expand.grid( .mtry = floor((length(preds_after_drop)-1) / 3), 
                              .min.node.size = 5,
                              .splitrule = "variance")
    set.seed(1982)
    
    fit <- train(
      pp,
      data            = dplyr::select(dfs, leafN, all_of(preds_after_drop)),
      metric          = "RMSE",
      method          = "ranger",
      tuneGrid        = tune_grid,
      trControl       = traincotrlParams,
      replace         = FALSE,
      sample.fraction = 0.5,
      num.trees       = 2000,        # boosted for the final model
      importance      = "impurity"   # for variable importance analysis, alternative: "permutation"
    )
    
    # add model object to list, and name the element according to the added variable
    fit_candidates[[ ipred ]] <- fit
    
    # record metrics for all candidates
    rsq <- fit$results$Rsquared
    df_rsq_candidates <- bind_rows(df_rsq_candidates, data.frame(pred = ipred, rsq = rsq))

  }
  
  ## get name of candidate predictor for which, when dropped, the model still achieved the highest R2.
  pred_drop <- df_rsq_candidates %>%  # when storing R2 in a data frame
    arrange(desc(rsq)) %>% 
    slice(1) %>% 
    pull(pred) %>% 
    as.character()
  
  rsq_new <- df_rsq_candidates %>%  # when storing R2 in a data frame
    arrange(desc(rsq)) %>% 
    slice(1) %>% 
    pull(rsq) %>% 
    as.numeric()

  print(paste("Dropping", pred_drop, " R2 = ", rsq_new))
  
  ## exit feature elimination once R2 drops below 0.45
  if (rsq_new < 0.45) break 
  
  ## drop next unnecessary predictor  
  preds_retained <- preds_retained[-which(preds_retained == pred_drop)]
  
  # record CV r2 of respective model
  df_metrics <- df_metrics %>% 
    bind_rows(
      data.frame( pred = pred_drop,
                  rsq = rsq_new
      )
    )
  
  # remove the selected variable from the candidate variable list
  preds_candidate <- preds_candidate[-which(preds_candidate == pred_drop)]

}

df_metrics
