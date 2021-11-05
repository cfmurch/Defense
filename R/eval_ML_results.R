#'
#'
#'
#' Call some of the metrics functions to see how we did on the eval set
#'
#'



eval_ML_results <- function(results_list, outcome, 
                            group_var = "time", id_var = "id",
                            metric_dict = metrics_data_dict,
                            cdr_ref = NULL){
  
  #Create a list with 
  pred_eval <- lapply(results_list, function(.hyper){
    
    #Initialize the NRI cuts as null
    cdr_ref_cut <- cdr_new_cut <- NULL
    
    #Extract the results df
    df <- .hyper[["results"]]
    
    #Pull the actual values and the predicted values (may generalize to pull the individual cross-fold holdouts)
    .actual <- df[["true"]]
    .pred <- df[["avg"]]
    
    #Add VISTIME for grouping
    dat <- data.frame(actual = .actual, pred = .pred, df[[group_var]])
    colnames(dat)[ncol(dat)] <- group_var
    if(outcome=="CDRSTAT") dat$avg_score <- df$avg_score
    
    #Pull the CDR ref scores for NRI if necessary (be sure to pull the correct IDs since different variable sets are being used for CDR)
    if(!is.null(cdr_ref)){
      dat$ref_score <- cdr_ref[[1]]$results$avg_score[cdr_ref[[1]]$results[[id_var]] %in% df$id]
      dat$new_score <- df$avg_score
      cdr_ref_cut <- unique(cdr_ref[[1]]$results$peak_mid)
      cdr_new_cut <- unique(df$peak_mid)
    }
    
    #Make sure there are no NA's in either result 
    dat <- dat[complete.cases(dat[,colnames(dat) %in% c("actual", "pred")]),]
    
    #Check if the evals are regression or classification - previously used a data dictionary which is a better option overall
    if(outcome == "ADASTOTAL11") { dict_curr <- metric_dict[["reg"]]
    } else dict_curr <- metric_dict[["class"]]
    
     
    #Can't get group_by and map_dfr to place nicely together, using lapply instead of df
    eval_curr <- lapply(sort(unique(dat[[group_var]])), function(.group){ 
      .out <- purrr::map_dfr(seq_along(dict_curr[["name"]]), ~run_eval_curr(.x, dat = dat[dat[[group_var]]==.group,], dict = dict_curr, 
                                                                            cut_old = cdr_ref_cut, cut_new = cdr_new_cut))
      .out <- cbind.data.frame(as.character(.group), .out)
    })
    
    #Bind to final dataframe
    eval_curr <- do.call(rbind, eval_curr)
    colnames(eval_curr)[1] <- group_var
    
    #Do the same evaluation for the full dataset
    eval_all <- cbind("All", purrr::map_dfr(seq_along(dict_curr[["name"]]), ~run_eval_curr(.x, dat, dict = dict_curr, 
                                                                                           cut_old = cdr_ref_cut, cut_new = cdr_new_cut)))
    colnames(eval_all)[1] <- group_var
    
    
    
    #Return the data frame for the current response
    return(list(Group = eval_curr, All = eval_all))
    
  })
  
  #Name the list of evaluations according to response and return
  hyper_names <- lapply(pred_eval, function(.hyper){.hyper[["hyperparams"]]})
  hyper_names <- do.call(c, hyper_names)
  names(pred_eval) <- hyper_names
  
  return(pred_eval)
  
  
  
}






run_eval_curr <- function(.idx, dat, dict, ...){
  
  cuts <- list(...)
  
  #Create evaluation string
  metric_string <- paste0("Metrics::", dict[["func"]][.idx], "(dat$actual, dat$pred)")
  
  #Use score for AUC if needed
  if(dict[["name"]][.idx] == "AUC" && length(which("avg_score" %in% colnames(dat)))>0) metric_string <- gsub("pred", "avg_score", metric_string)
  
  #Eval the string unless we're doing NRI in which case we just call the function
  if(dict[["name"]][.idx] == "NRI"){
    if(is.null(cuts$cut_old)) return(NULL)
    metric_curr <- nri_binom(dat, cuts$cut_old, cuts$cut_new)
  } else{
    metric_curr <- eval(parse(text = metric_string))
  }
  
  #A catch for AUC unless everything is only a single class in which case
  if(dict[["name"]][.idx] == "AUC" && is.nan(metric_curr) && length(table(dat$actual)>1)) metric_curr <- 1
  
  #Take abs of Bias if using AV Bias
  if(dict[["name"]][.idx] == "AV Bias") metric_curr <- median(abs(dat$actual - dat$pred))
  
  #Return as a dataframe
  data.frame(Metric = dict[["name"]][.idx], Value = metric_curr)
  
}



#This is the truncated version of the NRI binomial function

nri_binom <- function(dat, cut_old, cut_new){
  
  #Extract the relevant portions from the dataframe
  case_y <- dat$actual
  pred_old <- dat$ref_score
  pred_new <- dat$new_score
  
  #N of case/control for outcome Y
  n_case <- sum(case_y); n_ctrl <- length(case_y)-n_case
  
  #Build the prediction data frame in either case  
  df_pred <- data.frame(old=pred_old, new=pred_new, case=case_y)
  
  #Initialize new cases/controls, evaluate according to cut_old and cut_new
  df_pred$new_case <- 0; df_pred$new_case[df_pred$new>=cut_new & df_pred$old<cut_old] <- 1
  df_pred$new_ctrl <- 0; df_pred$new_ctrl[df_pred$new<cut_new & df_pred$old>=cut_old] <- 1
  
  #Determine number of improved/reduced cases and controls
  up_case <- sum(df_pred$new_case[df_pred$case==1])/n_case
  down_case <- sum(df_pred$new_ctrl[df_pred$case==1])/n_case
  up_ctrl <- sum(df_pred$new_case[df_pred$case==0])/n_ctrl
  down_ctrl <- sum(df_pred$new_ctrl[df_pred$case==0])/n_ctrl
  
  #Calculate the final NRI
  nri <- up_case - down_case + down_ctrl - up_ctrl
  
  #Set to a dataframe 
  new_plus <- data.frame(up_case, down_case, down_ctrl, up_ctrl, nri)
  
  #But only return the NRI
  return(nri)
  
}







#The original function for the network reclassification index
#Basically a copy of what's been used previously, we'll never use the glm builder option but the catch is still there
#The old versions will always be the reference unless a specific comparison is being done (so cut_old as the threshold and predict_old as the scores)
#We also will need the calcualted scores and the midpoint used for peak dichotomization since we don't have a {0,1} bound score
#And we'll always need case_y (the true categories)

nri_binom_orig <- function(new_x = NULL, old_x = NULL, case_y, cut_old, cut_new=NULL, predict_old = NULL, predict_new = NULL, use_predict = TRUE){
  
  #Determine cut-point for comparison (e.g. based on proportion of cases)
  if(is.null(cut_new)) cut_new <- cut_old
  
  #N of case/control for outcome Y
  n_case <- sum(case_y); n_ctrl <- length(case_y)-n_case
  
  
  #Check if building the models or using prior predictions
  if(use_predict == FALSE){
    
    #Build glm models based on different covariate sets
    lm_old <- glm(case_y ~ old_x, family=binomial(link=logit))
    lm_new <- glm(case_y ~ new_x, family=binomial(link=logit))
    
    #Calculate predicted probabilities for each model, set to dataframe along with known tags
    pred_new <- predict(lm_new, type="response"); pred_old <- predict(lm_old, type="response")
    
  } else{
    pred_new <- predict_new; pred_old <- predict_old
  }
  
  #Build the prediction data frame in either case  
  df_pred <- data.frame(old=pred_old, new=pred_new, case=case_y)
  
  #Initialize new cases/controls, evaluate according to cut_old and cut_new
  df_pred$new_case <- 0; df_pred$new_case[df_pred$new>=cut_new & df_pred$old<cut_old] <- 1
  df_pred$new_ctrl <- 0; df_pred$new_ctrl[df_pred$new<cut_new & df_pred$old>=cut_old] <- 1
  
  #Determine number of improved/reduced cases and controls
  up_case <- sum(df_pred$new_case[df_pred$case==1])/n_case
  down_case <- sum(df_pred$new_ctrl[df_pred$case==1])/n_case
  up_ctrl <- sum(df_pred$new_case[df_pred$case==0])/n_ctrl
  down_ctrl <- sum(df_pred$new_ctrl[df_pred$case==0])/n_ctrl
  
  #Calculate the final NRI
  nri <- up_case - down_case + down_ctrl - up_ctrl
  
  #Set to a dataframe and return
  new_plus <- data.frame(up_case, down_case, down_ctrl, up_ctrl, nri)
  return(new_plus)
}

















eval_ML_nn_results <- function(results_list, outcome, 
                            group_var = "time", id_var = "id",
                            metric_dict = metrics_data_dict,
                            cdr_ref = NULL){
  
  #Create a list with 
  pred_eval <- lapply(results_list, function(.hyper){
    
    #Initialize the NRI cuts as null
    cdr_ref_cut <- cdr_new_cut <- NULL
    
    #Extract the results df
    df <- .hyper[["results"]]
    
    #Pull the actual values and the predicted values (may generalize to pull the individual cross-fold holdouts)
    .actual <- df[["true"]]
    .pred <- df[["avg"]]
    
    #Add VISTIME for grouping
    dat <- data.frame(actual = .actual, pred = .pred, df[[group_var]])
    colnames(dat)[ncol(dat)] <- group_var
    
    #Pull the CDR ref scores for NRI if necessary
    if(!is.null(cdr_ref)){
      cdr_ref_curr <- cdr_ref[[1]]$results
      cdr_ref_curr <- cdr_ref_curr[cdr_ref_curr$id %in% df$id,]
      dat <- dat[which(df$id %in% cdr_ref_curr$id),]
      df <- df[df$id %in% cdr_ref_curr$id,]
      dat$ref_score <- cdr_ref_curr$avg_score
      dat$new_score <- df$avg_score
      cdr_ref_cut <- unique(cdr_ref_curr$peak_mid)
      cdr_new_cut <- unique(df$peak_mid)
    }
    
    #Make sure there are no NA's in either result 
    dat <- dat[complete.cases(dat),]
    
    #Check if the evals are regression or classification - previously used a data dictionary which is a better option overall
    if(outcome == "ADASTOTAL11") { dict_curr <- metric_dict[["reg"]]
    } else dict_curr <- metric_dict[["class"]]
    
    
    #Can't get group_by and map_dfr to place nicely together, using lapply instead of df
    eval_curr <- lapply(sort(unique(dat[[group_var]])), function(.group){ 
      .out <- purrr::map_dfr(seq_along(dict_curr[["name"]]), ~run_eval_curr(.x, dat = dat[dat[[group_var]]==.group,], dict = dict_curr, 
                                                                            cut_old = cdr_ref_cut, cut_new = cdr_new_cut))
      .out <- cbind.data.frame(as.character(.group), .out)
    })
    
    #Bind to final dataframe
    eval_curr <- do.call(rbind, eval_curr)
    colnames(eval_curr)[1] <- group_var
    
    #Do the same evaluation for the full dataset
    eval_all <- cbind("All", purrr::map_dfr(seq_along(dict_curr[["name"]]), ~run_eval_curr(.x, dat, dict = dict_curr, 
                                                                                           cut_old = cdr_ref_cut, cut_new = cdr_new_cut)))
    colnames(eval_all)[1] <- group_var
    
    
    
    #Return the data frame for the current response
    return(list(Group = eval_curr, All = eval_all))
    
  })
  
  #Name the list of evaluations according to response and return
  hyper_names <- lapply(pred_eval, function(.hyper){.hyper[["hyperparams"]]})
  hyper_names <- do.call(c, hyper_names)
  names(pred_eval) <- hyper_names
  
  return(pred_eval)
  
  
  
}














