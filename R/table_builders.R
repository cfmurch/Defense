#'
#' Functions to build tables via knitr
#'





table_proc_cont <- function(.tab, .type=FALSE, drop_2_5 = FALSE){
  
  .tab$Value <- round(.tab$Value, 3)
  if(drop_2_5 == TRUE) .tab <- .tab[-which(.tab$time == 2.5),]
  
  .out <- data.frame(Time=unique(.tab$time))
  .out$RMSE <- .tab$Value[.tab$Metric=="RMSE"]
  .out$MAE <- .tab$Value[.tab$Metric=="Median AE"]
  .out$Bias <- .tab$Value[.tab$Metric=="Bias"] * -1
  .out$`AV Bias` <- .tab$Value[.tab$Metric=="AV Bias"]
  if(.type==TRUE) colnames(.out)[1] <- "Type"
  
  rownames(.out) <- NULL
  return(.out)
}

summ_dict <- list(tree_names = c("MERF", "Single\nGLMM", "Bagged\nGLMM", "Boosted"),
                  nn_names = c("FNN", "1D CNN", "LSTM RNN"),
                  cont_vars = c("RMSE", "Median AE", "Bias", "AV Bias"),
                  cat_vars = c("Accuracy", "Precision", "Recall", "AUC"))

table_summ_build <- function(.dat, .ref){
  
  if(length(.dat)==3){ ml_names <- summ_dict[["nn_names"]]
  } else ml_names <- summ_dict[["tree_names"]]
  
  if("Bias" %in% .ref[[1]]$All$Metric){
    vars_curr <- summ_dict[["cont_vars"]]
    ml_names <- c("CPAD\nReference", ml_names)
  } else{
    vars_curr <- summ_dict[["cat_vars"]]
    ml_names <- c("CDR\nLogistic", ml_names)
  }
  
  .out <- .ref[[1]]$All
  .out <- .out[,-which(colnames(.out) == "time")]
  if("Bias" %in% .out$Metric) .out$Value[.out$Metric == "Bias"] <- .out$Value[.out$Metric == "Bias"] * -1
  .out$Value <- round(.out$Value, 3)
  .out <- .out[which(.out$Metric %in% vars_curr),]
  
  .out_ml <- lapply(.dat, function(.ml){
    .ml <- .ml[[1]]$All
    .ml <- .ml[which(.ml$Metric %in% vars_curr),]
    if("Bias" %in% .ml$Metric) .ml$Value[.ml$Metric == "Bias"] <- .ml$Value[.ml$Metric == "Bias"] * -1
    return(round(.ml$Value, 3))
  })
  .out_ml <- do.call(cbind.data.frame, .out_ml)
  .out <- cbind(.out, .out_ml)
  colnames(.out)[which(colnames(.out)!="Metric")] <- ml_names
  rownames(.out) <- NULL
  return(.out)
  
  
}










table_proc_cat <- function(.tab, .type=FALSE){
  
  .tab$Value <- round(.tab$Value, 4)
  
  .out <- data.frame(Time=unique(.tab$time))
  .out$Accuracy <- .tab$Value[.tab$Metric=="Accuracy"]
  .out$Precision <- .tab$Value[.tab$Metric=="Precision"]
  .out$Recall <- .tab$Value[.tab$Metric=="Recall"]
  .out$AUC <- .tab$Value[.tab$Metric=="AUC"]
  
  if("NRI" %in% .tab$Metric) .out$NRI <- .tab$Value[.tab$Metric=="NRI"]
  if(.type==TRUE) colnames(.out)[1] <- "Type"
  
  rownames(.out) <- NULL
  return(.out)
  
  
}


tab_proc_hyper <- function(.tab){
  .tab$Value <- round(.tab$Value, 4)
  .tab$Value[.tab$Metric=="Bias"] <- .tab$Value[.tab$Metric=="Bias"] *-1
  colnames(.tab) <- c("Metric", "Value", "Hyperparameters")
  
  if(length(grep("layer_count", .tab$Hyperparameters))>0){
    .tab$Hyperparameters <- gsub("(dense|cnn|lstm)_[1-3]_(activ|kern_constraint|bias_constraint|pool_size|kernel_size)(.*?)(;|NULL|relu)", "", .tab$Hyperparameters)
    .tab$Hyperparameters <- gsub("layer_count\\s*=\\s*\\d\\s*;\\s*", "", .tab$Hyperparameters)
    .tab$Hyperparameters <- gsub(";\\s*;", ";", .tab$Hyperparameters)
    .tab$Hyperparameters <- gsub("\\s*;\\s*;\\s*", " ; ", .tab$Hyperparameters)
    
    .tab$Hyperparameters <- gsub(";\\s*lstm_2.*", "", .tab$Hyperparameters)
    .tab$Hyperparameters <- gsub(";\\s*cnn_2.*", "", .tab$Hyperparameters)
    .tab$Hyperparameters <- gsub("(dense_\\d)_units", "\\1", .tab$Hyperparameters)
  }
  
  rownames(.tab) <- NULL
  return(.tab)
  
}

metric_list <- list(old_col = c("RMSE", "Median AE", "Bias", "AV Bias", "Accuracy", "Precision", "Recall", "AUC"),
                    new_col = c("RMSE", "MAE", "Bias", "AV Bias", "Accur", "Prec", "Recall", "AUC"))

tab_proc_boot <- function(.boot, .pred, .ref, .boot_ref, drop_time = TRUE, abs_bias = TRUE, use_boot_avg = FALSE){
  .boot_dat <- .boot$boot_data[.boot$boot_data$time=="All",]
  .boot_ref <- .boot_ref[.boot_ref$time=="All",]
  .boot <- .boot$results[.boot$results$time == "All",]
  
  .boot$Diff <- round(.boot$Diff, 3)
  .boot$Prop_diff <- gsub(" ", "", .boot$Prop_diff)
  .boot$Boot_p <- gsub(" ", "", .boot$Boot_p)
  
  
  if(use_boot_avg == FALSE){
    if("Bias" %in% colnames(.boot_dat)){
      .pred$Value[.pred$Metric == "Bias"] <- .pred$Value[.pred$Metric == "Bias"] * -1
      .ref$Value[.ref$Metric == "Bias"] <- .ref$Value[.ref$Metric == "Bias"] * -1
      .boot$Diff[.boot$Metric == "Bias"] <- .boot$Diff[.boot$Metric == "Bias"] * -1
      .temp_ci <- .boot$CI_l[.boot$Metric == "Bias"] * -1
      .boot$CI_l[.boot$Metric == "Bias"] <- .boot$CI_u[.boot$Metric == "Bias"] * -1
      .boot$CI_u[.boot$Metric == "Bias"] <- .temp_ci
    }
    pred_avg <- .pred$Value <- round(.pred$Value, 3)
    ref_avg <- .ref$Value <- round(.ref$Value, 3)
  } else{
    if("Bias" %in% colnames(.boot_dat)){
      #.boot_dat$Bias <- abs(.boot_dat$Bias)
      #.boot_ref$Bias <- .boot_ref$`AV Bias`
      #.boot_ref <- .boot_ref[,-which(colnames(.boot_ref) == "AV Bias")]
    }
    pred_avg <- apply(.boot_dat[,colnames(.boot_dat) %not_in% c("Boot", "time")], 2, mean)
    ref_avg <- apply(.boot_ref[,colnames(.boot_dat) %not_in% c("Boot", "time")], 2, mean)
    if("Bias" %in% colnames(.boot_dat)){
      pred_avg[which(colnames(.boot_dat) %not_in% c("Boot", "time") == "AV Bias")] <- median(abs(.boot_dat$`AV Bias`))
      ref_avg[which(colnames(.boot_ref) %not_in% c("Boot", "time") == "AV Bias")] <- median(abs(.boot_ref$`AV Bias`))
    }
    .pred$Value <- round(pred_avg, 3)
    .ref$Value <- round(ref_avg, 3)
  }
  .boot$ci <- paste0("[", sprintf("%.3f", .boot$CI_l), ", ", sprintf("%.3f", .boot$CI_u), "]")
  
  
  if("Bias" %in% .pred$Metric){
    .pred$Value[.pred$Metric]
  }
  
  if("NRI" %in% .pred$Metric){
    nri_curr <- paste0("NRI: ", .pred$Value[.pred$Metric == "NRI"])
  }
  
  .boot <- .boot[.boot$Metric %in% metric_list$old_col,]
  .pred <- .pred[.pred$Metric %in% metric_list$old_col,]
  .ref <- .ref[.ref$Metric %in% metric_list$old_col,]
  
  .boot$Metric <- metric_list$new_col[which(metric_list$old_col %in% .boot$Metric)]
  .pred$Metric <- metric_list$new_col[which(metric_list$old_col %in% .pred$Metric)]
  .ref$Metric <- metric_list$new_col[which(metric_list$old_col %in% .ref$Metric)]
  
  
  if(drop_time == TRUE){
    .boot <- .boot[,-which(colnames(.boot)=="time")]
    .pred <- .pred[,-which(colnames(.pred)=="time")]
    .ref <- .ref[,-which(colnames(.ref)=="time")]
  }
  
  
  .out <- data.frame(Metric = .pred$Metric, V2 = .pred$Value, V3 = .ref$Value, .boot[,colnames(.boot) %in% c("Diff", "Prop_diff", "ci")])
  colnames(.out) <- c("Metric", "ML Model", "Ref Model", "Delta Boot", "Perc Diff", "95% CI<br>Boot")
  rownames(.out) <- NULL
  
  if(exists("nri_curr")){
    return(list(tab = .out, NRI = nri_curr))
  } else  return(list(tab = .out))
}








remake_comp_table <- function(.tab){
  .tab[lower.tri(.tab)] = NA_real_
  .tab <- cbind(NA, .tab)
  for(i in c(1:nrow(.tab))){
    .tab[i,i] <- paste0("<b>",rownames(.tab)[i],"</b>")
  }
  rownames(.tab) <- NULL
  image_string <- paste0(dir.base, "Image/pretty_tree.png")
  #.tab[nrow(.tab),1] <- paste0("<img src=\"", image_string, "\" alt=\"\">")
  #.tab[nrow(.tab),1] <- "<img src='https://upload.wikimedia.org/wikipedia/commons/d/dd/Square_-_black_simple.svg'>"
  .tab
}





table1_build <- function(.dat, .dict=tab1_dict){
  
  dat_out_list <- 
  lapply(.dict[["var_names"]], function(.var){
    if(.var %in% .dict[["cat_var"]]){
      cdr_tot_0 <- length(.dat[[.var]][!is.na(.dat[[.var]]) & !is.na(.dat$CDRSTAT) & .dat$CDRSTAT==0])
      cdr_tot_1 <- length(.dat[[.var]][!is.na(.dat[[.var]]) & !is.na(.dat$CDRSTAT) & .dat$CDRSTAT==1])
      cdr_tot_all <- length(.dat[[.var]][!is.na(.dat[[.var]]) & !is.na(.dat$CDRSTAT)])
      dat_out <- 
      lapply(sort(na.omit(unique(.dat[[.var]]))), function(ii){
        cdr_0 <- length(.dat[[.var]][!is.na(.dat[[.var]]) & !is.na(.dat$CDRSTAT) &  .dat$CDRSTAT==0 & .dat[[.var]]==ii])
        cdr_1 <- length(.dat[[.var]][!is.na(.dat[[.var]]) & !is.na(.dat$CDRSTAT) &  .dat$CDRSTAT==1 & .dat[[.var]]==ii])
        cdr_all <- cdr_0 + cdr_1
        if(ii==0) return(NULL)
        string_all <- paste0(cdr_all, "<br>(", sprintf("%.1f", cdr_all/cdr_tot_all*100), ")")
        string_0 <- paste0(cdr_0, "<br>(", sprintf("%.1f", cdr_0/cdr_tot_0*100), ")")
        string_1 <- paste0(cdr_1, "<br>(", sprintf("%.1f", cdr_1/cdr_tot_1*100), ")")
        dat_out <- data.frame(All = string_all, CDR_0 = string_0, CDR_1 = string_1)
        return(dat_out)
      })
      dat_out <- data.frame(Var = .dict[["multilevel_names"]][[which(.dict[["cat_var"]] == .var)]][-1], do.call(rbind, dat_out))
      
    } else{
      if(.var == "AGE") .dat[[.var]] <- .dat[[.var]] + 75
      string_all <- paste0(sprintf("%.1f", mean(.dat[[.var]][!is.na(.dat[[.var]]) & !is.na(.dat$CDRSTAT)])), " \u00b1<br>", sprintf("%.2f", sd(.dat[[.var]][!is.na(.dat[[.var]]) & !is.na(.dat$CDRSTAT)])))
      string_0 <- paste0(sprintf("%.1f", mean(.dat[[.var]][!is.na(.dat[[.var]]) & !is.na(.dat$CDRSTAT) & .dat$CDRSTAT==0])), " \u00b1<br>", sprintf("%.2f", sd(.dat[[.var]][!is.na(.dat[[.var]]) & !is.na(.dat$CDRSTAT) & .dat$CDRSTAT==0])))
      string_1 <- paste0(sprintf("%.1f", mean(.dat[[.var]][!is.na(.dat[[.var]]) & !is.na(.dat$CDRSTAT) & .dat$CDRSTAT==1])), " \u00b1<br>", sprintf("%.2f", sd(.dat[[.var]][!is.na(.dat[[.var]]) & !is.na(.dat$CDRSTAT) & .dat$CDRSTAT==1])))
      dat_out <- data.frame(Var = .dict[["new_names"]][which(.dict[["cont_var"]]==.var)], All = string_all, CDR_0 = string_0, CDR_1 = string_1)
    }
    return(dat_out)
    
  })
  dat_final <- do.call(rbind.data.frame, dat_out_list)
  rownames(dat_final) <- NULL
  N_cdr_0 <- length(.dat$CDRSTAT[!is.na(.dat$CDRSTAT) & .dat$CDRSTAT==0])
  N_cdr_1 <- length(.dat$CDRSTAT[!is.na(.dat$CDRSTAT) & .dat$CDRSTAT==1])
  N_tot <- N_cdr_0 + N_cdr_1
  colnames(dat_final) <- c("Covariate", paste0("All Subjects<br>N = ", N_tot), paste0("CDR 0 Subjects<br>N = ", N_cdr_0), paste0("CDR 0.5+ Subjects<br>N = ", N_cdr_1))
  return(dat_final)

}
 
  
  
  
  


tab1_dict <- list(var_names = c("ADASTOTAL11", "AGE", "SEX", "APOE", "RACE", "ETHNIC", "EDUCFAC", "MMSCORE", "ADMED", "WEIGHT", "BPSYS", "BPDIA"),
                  cont_var = c("ADASTOTAL11", "AGE", "MMSCORE", "WEIGHT", "BPSYS", "BPDIA"),
                  new_names = c("BL ADAS-Cog<br>Score", "BL Age (yrs)", "BL MMSE<br>Score", "BL Weight (lbs)", "BL Systolic<br>Blood Pressure", "BL Diastolic<br>Blood Pressure"),
                  cat_var = c("SEX", "APOE", "RACE", "ETHNIC", "EDUCFAC", "ADMED"), 
                  multilevel_names = list(c("Male", "Female"),
                                          c("Non-carrier", "Heterozygous<br>APOE4 Carriers", "Homozygous<br>APOE4 Carriers"),
                                          c("White", "Black / African<br>American", "Other Race"),
                                          c("Non-Hispanic<br>or Unknown", "Hispanic"),
                                          c("Less than HS", "High School<br>Diploma or GED", "Some college", "College degree", "Some post-grad", "Graduate degree"),
                                          c("No AD medication", "Anti-dementia<br>medication")))


