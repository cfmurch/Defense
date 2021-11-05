#'
#'
#' Functions to help with the random effects imputation presentations
#'
#'



p_tabler <- function(x1, x2, .var, .all = TRUE){
  if(.all == TRUE){.group <- "All"
  } else .group <- "Group"
  
  if(.var == "RMSE"){
    x1 <- eval_process[[x1]][[.group]][[.var]]
    x2 <- eval_process[[x2]][[.group]][[.var]]
    
    prop_curr <- sign(x1 - x2)
    prop_curr[prop_curr == -1] <- 0
    prop_curr <- mean(prop_curr)
    
    avg_x1 <- mean(x1); dev_x1 <- sd(x1)
    avg_x2 <- mean(x2); dev_x2 <- sd(x2)
    out_x1 <- paste0(sprintf("%.2f", avg_x1), " ± ", sprintf("%.3f", dev_x1))
    out_x2 <- paste0(sprintf("%.2f", avg_x2), " ± ", sprintf("%.3f", dev_x2))
    
    test_curr <- t.test(x1, x2)
    test_p <- test_curr$p.value
    if(test_p < 0.001){ test_p <- "<0.001"
    } else test_p <- signif(test_p, 2)
    
    prop_diff <- abs(mean(x1)-mean(x2))/min(mean(x1), mean(x2))
    prop_diff <- paste0(sprintf("%.1f", prop_diff*100), "%")
    
  }
  
  if(.var == "Bias"){
    x1 <- abs(eval_process[[x1]][[.group]][[.var]])
    x2 <- abs(eval_process[[x2]][[.group]][[.var]])
    
    prop_curr <- sign(x1 - x2)
    prop_curr[prop_curr == -1] <- 0
    prop_curr <- mean(prop_curr)
    
    avg_x1 <- median(x1); dev_x1 <- IQR(x1)
    avg_x2 <- median(x2); dev_x2 <- IQR(x2)
    out_x1 <- paste0(sprintf("%.2f", avg_x1), " ± ", sprintf("%.3f", dev_x1))
    out_x2 <- paste0(sprintf("%.2f", avg_x2), " ± ", sprintf("%.3f", dev_x2))
    
    test_curr <- wilcox.test(x1, x2)
    test_p <- test_curr$p.value
    if(test_p < 0.001){ test_p <- "<0.001"
    } else test_p <- signif(test_p, 2)
    
    prop_diff <- abs(median(x1)-median(x2))/min(median(x1), median(x2))
    prop_diff <- paste0(sprintf("%.1f", prop_diff*100), "%")
    
  }
  
  writeLines(paste0(out_x1, "\n", out_x2, "\n", paste0(prop_diff, " (", test_p, ")"), "\n", prop_curr))
  
}





string_build <- function(.dat, .var, .all = TRUE){
  if(.all == TRUE){.group <- "All"
  } else .group <- "Group"
  
  if(.var == "Bias"){
    dat_curr <- abs(.dat[[.group]][[.var]])
    avg_curr <- median(dat_curr)
    dev_curr <- IQR(dat_curr)
    
  } else{
    
    dat_curr <- .dat[[.group]][[.var]]
    avg_curr <- mean(dat_curr)
    dev_curr <- sd(dat_curr)
  }
  
  str_curr <- paste0(sprintf("%.2f", avg_curr), " ", "\u00B1", "<br>", sprintf("%.3f", dev_curr))
  return(str_curr)
  
  
}



impute_dict = list(ml = c("ref", "beta", "merf"),
                   eval = c("subj_noRE_eval", "subj_impute_eval", "obs_noRE_eval", "obs_impute_eval", "obs_fit_eval"),
                   metric = c("RMSE", "Median.AE", "Bias"),
                   metric_name = c("RMSE", "Median AE", "AV Bias"),
                   
                   str_col_names = c("CPAD<br>Parameters", "De novo<br>BR Model", "De Novo<br>MERF Model"),
                   impute_type = c("Fixed Effects", "Imputed<br>Random Effects", "Fixed Effects", "Imputed<br>Random Effects", "Fitted<br>Random Effects")
                   
                   )
                   




#Function for building the table for displaying mean metrics and variations

#eval_curr <- eval_true; eval_curr <- eval_synth

dir.base <- "E:/UAB SOPH/Dissertation/Full Project - Meta Database/"

#eval_curr <- eval_true; eval_curr <- eval_synth

impute_table_builder <- function(eval_curr){
  outer_ml <- 
    lapply(impute_dict$metric, function(.var){
      inner_var <- 
        lapply(impute_dict$ml, function(.ml){
          inner_mod <- 
            lapply(impute_dict$eval, function(.eval){
              entry_curr <- paste0(.ml, "_", .eval)
              if(.ml == "ref" && .eval == "obs_fit_eval") return(NA)
              string_build(eval_curr[[entry_curr]], .var)
            })
          do.call(c, inner_mod)
        })
      do.call(cbind.data.frame, inner_var)
    })
  outer_ml <- do.call(cbind.data.frame, outer_ml)
  
  outer_ml <- cbind(V1 = impute_dict$impute_type, outer_ml)
  colnames(outer_ml) <- c("Random Effects<br>Design", rep(impute_dict$str_col_names, 3))
  rownames(outer_ml) <- NULL
  
  return(outer_ml)
}



impute_comp_dict <- list(list_names = c("ref_subj_noRE_eval", "ref_subj_impute_eval", "ref_obs_noRE_eval", "ref_obs_impute_eval",
                                        "beta_subj_noRE_eval", "beta_subj_impute_eval", "beta_obs_noRE_eval", "beta_obs_impute_eval", "beta_obs_fit_eval",
                                        "merf_subj_noRE_eval", "merf_subj_impute_eval", "merf_obs_noRE_eval", "merf_obs_impute_eval", "merf_obs_fit_eval"),
                         col_names = c("CPAD<br>FE Only", "CPAD<br>Imputed", "CPAD<br>FE Only", "CPAD<br>Imputed", 
                                       "De Novo BR<br>FE Only", "De Novo BR<br>Imputed", "De Novo BR<br>FE Only", "De Novo BR<br>Imputed", "De Novo BR<br>Fitted",
                                       "De Novo MERF<br>FE Only", "De Novo MERF<br>Imputed", "DN MERF<br>FE Only", "DN MERF<br>Imputed", "DN MERF<br>Fitted"))


#Function for building out the comparison table of proportional differences


str_match_check <- function(x1, x2){
  x1 <- gsub("(subj|obs)_", "", x1); x2 <- gsub("(subj|obs)_", "", x2)
  x1 <- gsub("_eval", "", x1); x2 <- gsub("_eval", "", x2)
  x1 <- strsplit(x1, "_")[[1]]
  x2 <- strsplit(x2, "_")[[1]]
  string_check <- na.omit(pmatch(x1, x2))
  if(length(string_check)>0){ return(TRUE)
  } else return(FALSE)
}


impute_compare_builder <- function(.eval, bold_set, obs = FALSE, .all = TRUE){
  
  if(.all == TRUE){.group <- "All"
  } else .group <- "Group"
  
  if(obs == TRUE){ eval_set <- grep("_obs_", names(.eval), value = TRUE)
  } else eval_set <- grep("_subj_", names(.eval), value = TRUE)
  
  comp_table <- matrix(NA, ncol=length(eval_set), nrow = length(eval_set))
  if(obs == TRUE ) {deg_curr <- "25"
  } else deg_curr <- "20"
  
  for(i in seq_len(length(eval_set))){
    for(j in seq_len(length(eval_set))){
      
      if(j == i){
        if(obs == TRUE){
          #comp_table[i,j] <- paste0("<hr><style>hr{transform: rotate(", 25, "deg)}</style>")
          comp_table[i,j] <- "<hr style=\"transform: rotate(32deg)\"></hr>"
        } else {
          #comp_table[i,j] <- paste0("<hr><style>hr{transform: rotate(", 0, "deg)}</style>")
          comp_table[i,j] <- "<hr style=\"transform: rotate(27deg)\"></hr>"
        }
      }
      
      if(j > i && str_match_check(eval_set[i], eval_set[j]) == TRUE){
        
        x1_rmse <- .eval[[eval_set[i]]][[.group]][["RMSE"]]
        x2_rmse <- .eval[[eval_set[j]]][[.group]][["RMSE"]]
        
        prop_rmse <- sign(x1_rmse - x2_rmse)
        prop_rmse[prop_rmse == -1] <- 0
        prop_rmse <- mean(prop_rmse)
        if(prop_rmse > 0.5) prop_rmse <- 1 - prop_rmse
        if(prop_rmse == 0){ prop_rmse <- "<0.001"
        } else prop_rmse <- sprintf("%.2f", signif(prop_rmse, 2))
        
        prop_diff_rmse <- (mean(x2_rmse)-mean(x1_rmse))/min(mean(x1_rmse), mean(x2_rmse))
        prop_diff_rmse <- paste0(sprintf("%.1f", prop_diff_rmse*100), "%")
        
        prop_diff_rmse_ci <- ci_boot_func_impute(x1_rmse, x2_rmse, .bias = FALSE)
        
        test_rmse <- t.test(x1_rmse, x2_rmse)
        test_p_rmse <- test_rmse$p.value
        if(test_p_rmse < 0.001){ test_p_rmse <- "<0.001"
        } else test_p_rmse <- sprintf("%.2f", signif(test_p_rmse, 2))
        
        #string_rmse <- paste0(prop_diff_rmse, "<br>(", prop_rmse, ")")
        string_rmse <- paste0(prop_diff_rmse, "<br>[", prop_diff_rmse_ci, "]")
        #if(prop_rmse <= 0.1) string_rmse <- paste0("<b>", string_rmse, "</b>")
        
        
        x1_bias <- abs(.eval[[eval_set[i]]][[.group]][["Bias"]])
        x2_bias <- abs(.eval[[eval_set[j]]][[.group]][["Bias"]])
        
        prop_bias <- sign(x1_bias - x2_bias)
        prop_bias[prop_bias == -1] <- 0
        prop_bias <- mean(prop_bias)
        if(prop_bias > 0.5) prop_bias <- 1 - prop_bias
        if(prop_bias == 0){ prop_bias <- "<0.001"
        } else prop_bias <- sprintf("%.2f", signif(prop_bias, 2))
        
        prop_diff_bias <- (median(x1_bias)-median(x2_bias))/min(median(x1_bias), median(x2_bias))
        prop_diff_bias <- paste0(sprintf("%.1f", prop_diff_bias*100), "%")
        
        prop_diff_bias_ci <- ci_boot_func_impute(x2_bias, x1_bias, .bias = TRUE)
        
        test_bias <- wilcox.test(x1_bias, x2_bias)
        test_p_bias <- test_bias$p.value
        if(test_p_bias < 0.001){ test_p_bias <- "<0.001"
        } else test_p_bias <- sprintf("%.2f", signif(test_p_bias, 2))
        
        #string_bias <- paste0(prop_diff_bias, "<br>(", prop_bias, ")")
        string_bias <- paste0(prop_diff_bias, "<br>[", prop_diff_bias_ci, "]")
        #if(prop_bias <= 0.1) string_bias <- paste0("<b>", string_bias, "</b>")
        
        #Populate the matrix in comp_table
        comp_table[j,i] <- string_rmse; comp_table[i,j] <- string_bias
      }
    }
  }
  
  for(qq in seq_len(nrow(bold_set))){
    comp_table[bold_set$rows[qq],bold_set$cols[qq]] <- paste0("<b>", comp_table[bold_set$rows[qq],bold_set$cols[qq]], "</b>")
  }
  
  colnames(comp_table) <- rownames(comp_table) <- impute_comp_dict[["col_names"]][which(impute_comp_dict[["list_names"]] %in% eval_set)]
  return(comp_table)
}



bold_dict <- list(traj_meta = data.frame(rows = c(2,1,4,6,4,2,1,6,2),
                                         cols = c(1,2,2,2,3,4,5,5,6)),
                  obs_meta = data.frame(rows = c(2,1,4,7,4,5,2,5,7,3,4,8,7,8,2,8,6),
                                        cols = c(1,2,2,2,3,3,4,4,4,5,5,5,6,6,7,7,8)),
                  traj_synth = data.frame(rows = c(2,4,6,4,6),
                                          cols = c(1,2,2,3,5)),
                  obs_synth = data.frame(rows = c(2,4,7,4,5,5,3,7,8,8,6),
                                         cols = c(1,2,2,3,3,4,5,6,6,7,8)))




impute_plot_dict <- list(type = c("subj", "obs"),
                         levels = list(c("ref_subj_noRE_eval", "ref_subj_impute_eval", "beta_subj_noRE_eval", "beta_subj_impute_eval", "merf_subj_noRE_eval", "merf_subj_impute_eval"),
                                       c("ref_obs_noRE_eval", "ref_obs_impute_eval", "beta_obs_noRE_eval", "beta_obs_impute_eval", "beta_obs_fit_eval", "merf_obs_noRE_eval", "merf_obs_impute_eval", "merf_obs_fit_eval")),
                         labels = list(c("CPAD\nfixed only", "CPAD\nimputed", "DN BR\nfixed only", "DN BR\nimputed", "MERF\nfixed only", "MERF\nimputed"),
                                       c("CPAD\nfixed only", "CPAD\nimputed", "DN BR\nfixed only", "DN BR\nimputed", "DN BR\nfitted", "MERF\nfixed only", "MERF\nimputed", "MERF\nfitted")),
                         size_set = c(1, 0.9))





impute_plot_builder <- function(.eval, dict_type){
  
  dict_idx <- which(impute_plot_dict$type == dict_type)
  
  df_plot_all <- lapply(names(.eval), function(.name){
    data.frame(Type = as.character(.name), rmse = .eval[[.name]]$All$RMSE, bias = .eval[[.name]]$All$Bias)})
  df_plot_all <- do.call(rbind.data.frame, df_plot_all)
  df_plot_all <- df_plot_all[grep(dict_type, df_plot_all$Type),]
  df_plot_all$Type <- factor(df_plot_all$Type, levels = impute_plot_dict[["levels"]][[dict_idx]], labels = impute_plot_dict[["labels"]][[dict_idx]])
  
  size_curr <- impute_plot_dict[["size_set"]][dict_idx]
  
  plot_rmse <- 
    ggplot(data=df_plot_all, aes(x = Type, y = rmse)) + 
    geom_point(aes(color = Type), position = position_jitter(width = 0.25, height = 0), size=size_curr, show.legend = FALSE, alpha=0.7) +
    geom_errorbar(aes(group = Type), stat = "summary", fun.data = "mean_sdl", fun.args = list(mult = 1), width = 0.2, size = 1.25, alpha = 0.85, show.legend = FALSE) + 
    stat_summary(aes(group = Type), fun = mean, geom = "point", shape = 45, size = 18, show.legend = FALSE) + 
    
    #scale_color_manual(values = RColorBrewer::brewer.pal(3, "Dark2")[1:2], labels=c("Actual", "Predicted")) + 
    scale_y_continuous(name = "Root Mean Square Error", breaks = seq(0,50,2)) + 
    
    theme_explorer + theme(axis.title.x = element_blank())
  
  
  plot_bias <- 
    ggplot(data=df_plot_all, aes(x = Type, y = abs(bias))) + 
    geom_point(aes(color = Type), position = position_jitter(width = 0.25, height = 0), size=size_curr, show.legend = FALSE, alpha = 0.7) +
    geom_errorbar(aes(group = Type), stat = "summary", fun.data = "median_hilow", fun.args = list(conf.int = 0.75), width = 0.2, size = 1.25, alpha = 0.85, show.legend = FALSE) + 
    stat_summary(aes(group = Type), fun = mean, geom = "point", shape = 45, size = 18, show.legend = FALSE) + 
    
    #scale_color_manual(values = RColorBrewer::brewer.pal(3, "Dark2")[1:2], labels=c("Actual", "Predicted")) + 
    scale_y_continuous(name = "Absolute Value of Bias", breaks = seq(0,10,1)) + 
    
    theme_explorer + theme(axis.title.x = element_blank())
  
  
  return(list(plot_rmse, plot_bias))
  
}





