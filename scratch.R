


R.utils::sourceDirectory("./R/", modifiedOnly = FALSE)




set.seed(654321)

dir_base <- "E:/UAB SOPH/Dissertation/Full Project - Meta Database/"

dir_ref <- paste0(dir_base, "MERF/From Cheaha/_targets/objects/")
ref_adas <- readRDS(paste0(dir_ref, "refmod_adas_holdout_eval")); ref_adas <- rbind(ref_adas[[1]]$Group, ref_adas[[1]]$All)
ref_cdr <- readRDS(paste0(dir_ref, "refmod_cdr_holdout_eval")); ref_cdr <- rbind(ref_cdr[[1]]$Group, ref_cdr[[1]]$All)
ref_adas_obs <- readRDS(paste0(dir_ref, "refmod_adas_holdout_eval_obs_noRE")); ref_adas_obs <- rbind(ref_adas_obs[[1]]$Group, ref_adas_obs[[1]]$All)
ref_cdr_obs <- readRDS(paste0(dir_ref, "refmod_cdr_holdout_eval_obs_noRE")); ref_cdr_obs <- rbind(ref_cdr_obs[[1]]$Group, ref_cdr_obs[[1]]$All)

ref_adas_boot <- boot_func(.file = paste0(dir_ref, "refmod_adas_holdout_results"), .out = "ADASTOTAL11", boot_only = TRUE)
ref_cdr_boot <- boot_func(.file = paste0(dir_ref, "refmod_cdr_holdout_results"), .out = "CDRSTAT", boot_only = TRUE)
ref_adas_boot_obs <- boot_func(.file = paste0(dir_ref, "refmod_adas_holdout_results_obs_noRE"), .out = "ADASTOTAL11", boot_only = TRUE)
ref_cdr_boot_obs <- boot_func(.file = paste0(dir_ref, "refmod_cdr_holdout_results_obs_noRE"), .out = "CDRSTAT", boot_only = TRUE)
boot_ref <- list(adas = ref_adas_boot, cdr = ref_cdr_boot, adas_obs = ref_adas_boot_obs, cdr_obs = ref_cdr_boot_obs)
saveRDS(boot_ref, "boot_ref.rds")

# eval_true <- readRDS(paste0(dir_base, "RandEff Impute/impute_true.rds"))
# xx <- eval_true$merf_obs_fit_eval$All$Bias; xx2 <- eval_true$merf_obs_fit_eval$Group$Bias
# yy <- eval_true$merf_obs_impute_eval$All$Bias; yy2 <- eval_true$merf_impute_fit_eval$Group$Bias
# eval_true$merf_obs_impute_eval$All$Bias <- eval_true$merf_obs_noRE_eval$All$Bias
# eval_true$merf_obs_impute_eval$Group$Bias <- eval_true$merf_obs_noRE_eval$Group$Bias
# eval_true$merf_obs_noRE_eval$All$Bias <- xx; eval_true$merf_obs_noRE_eval$Group$Bias <- xx2
# eval_true$merf_obs_fit_eval$All$Bias <- yy; eval_true$merf_obs_fit_eval$Group$Bias <- yy2
# saveRDS(eval_true, paste0(dir_base, "RandEff Impute/impute_true.rds"))





boot_dict <- list(ML = c("merf", "glmertree_ref", "glmertree_bagged", "mvtboost", "fnn", "cnn", "lstm"),
                  dir = c("MERF/From Cheaha/_targets/objects/",
                          "MERF_glmer/GLMER_ref/",
                          "MERF_glmer/Assembly/",
                          "MERF_mvt/From Cheaha/_targets/objects/",
                          rep("Neural Nets/_targets/objects/", 3)),
                  names = c("MERF", "GLMER", "Bagged", "Boosted", "FNN", "CNN", "LSTM"))

holdout_dict <- list(hold = c("adas", "cdr", "adas_obs", "cdr_obs"),
                     header = c("adas", "cdr", "adas", "cdr"),
                     obs = c("", "", "_obs", "_obs"),
                     outcome = c("ADASTOTAL11", "CDRSTAT", "ADASTOTAL11", "CDRSTAT"),
                     comp_ref = c(quote(ref_adas), quote(ref_cdr), quote(ref_adas_obs), quote(ref_cdr_obs)),
                     comp_boot = c(quote(ref_adas_boot), quote(ref_cdr_boot), quote(ref_adas_boot_obs), quote(ref_cdr_boot_obs)))

lower_is_better_eval <- c("RMSE", "Median AE", "Symm MAE Percent", "Bias", "AV Bias", "Class Error")

comparison_metrics <- c("RMSE", "Median AE", "Bias", "AV Bias", "Accuracy", "Precision", "Recall", "AUC")


#Part 1 - Comparing models against the reference

set.seed(654321)
dat_final <- lapply(seq_along(1:length(boot_dict[["ML"]])), function(.ml){
  #Prep the directory we're bootstrapping
  ml_curr <- boot_dict[["ML"]][.ml]
  dir_curr <- paste0(dir_base, boot_dict[["dir"]][.ml])
  print(ml_curr)
  
  #We iterate over each of the holdout types
  dat_ml <- lapply(seq_along(1:length(holdout_dict[["hold"]])), function(.hold){
    
    print(holdout_dict[["hold"]][[.hold]])
    
    #Build the argument sets and strings to pass to the boot function
    rds_curr <- paste0(ml_curr, "_", holdout_dict[["header"]][.hold], "_", "holdout_results", holdout_dict[["obs"]][.hold])
    rds_eval <- paste0(ml_curr, "_", holdout_dict[["header"]][.hold], "_", "holdout_eval", holdout_dict[["obs"]][.hold])
    
    results_curr <- readRDS(paste0(dir_curr, rds_curr))
    true_dat_eval <- readRDS(paste0(dir_curr, rds_eval)); true_dat_eval <- rbind(true_dat_eval[[1]]$Group, true_dat_eval[[1]]$All)
    outcome <- holdout_dict[["outcome"]][.hold]
    
    
    
    boot_test_curr <- boot_func(dat = results_curr, ref_boot = eval(holdout_dict[["comp_boot"]][.hold][[1]]), .out = outcome,
                                true_ref = eval(holdout_dict[["comp_ref"]][.hold][[1]]), true_dat = true_dat_eval)
    
    return(boot_test_curr)
    
  })
  
  names(dat_ml) <- holdout_dict$hold
  return(dat_ml)
})

names(dat_final) <- boot_dict$ML


saveRDS(dat_final, "boot_data.rds")







set.seed(654321)
dat_full_comp <- lapply(seq_len(length(holdout_dict[["hold"]])), function(.hold){
  outcome <- holdout_dict[["outcome"]][.hold]
  
  dat_comp_outer <- lapply(seq_len(length(boot_dict[["ML"]])), function(.ml_1){
  
    ml_1 <- boot_dict[["ML"]][.ml_1]
    dir_1 <- paste0(dir_base, boot_dict[["dir"]][.ml_1])
    
    rds_1 <- paste0(ml_1, "_", holdout_dict[["header"]][.hold], "_", "holdout_results", holdout_dict[["obs"]][.hold])
    rds_eval_1 <- paste0(ml_1, "_", holdout_dict[["header"]][.hold], "_", "holdout_eval", holdout_dict[["obs"]][.hold])
    
    results_1 <- readRDS(paste0(dir_1, rds_1))
    true_eval_1 <- readRDS(paste0(dir_1, rds_eval_1)); true_eval_1 <- rbind(true_eval_1[[1]]$Group, true_eval_1[[1]]$All)
    
    dat_comp_ml1 <- lapply(seq_len(length(boot_dict[["ML"]])), function(.ml_2){
    
      if(.ml_2 == .ml_1) return()
    
      ml_2 <- boot_dict[["ML"]][.ml_2]
      dir_2 <- paste0(dir_base, boot_dict[["dir"]][.ml_2])
      
      rds_2 <- paste0(ml_2, "_", holdout_dict[["header"]][.hold], "_", "holdout_results", holdout_dict[["obs"]][.hold])
      rds_eval_2 <- paste0(ml_2, "_", holdout_dict[["header"]][.hold], "_", "holdout_eval", holdout_dict[["obs"]][.hold])
      
      results_2 <- readRDS(paste0(dir_2, rds_2))
      true_eval_2 <- readRDS(paste0(dir_2, rds_eval_2)); true_eval_2 <- rbind(true_eval_2[[1]]$Group, true_eval_2[[1]]$All)
     
      #if(.ml_2 < .ml_1){
        boot_test_curr <- boot_func(dat = results_1, ref_boot = NULL, .out = outcome,
                                    true_ref = true_eval_2, true_dat = true_eval_1, return_res = FALSE)
      #}
      
      return(boot_test_curr)
      
    })
    
    
  })
  
  
})

names(dat_full_comp) <- holdout_dict$hold
for(i in seq_along(1:length(dat_full_comp))){
  names(dat_full_comp[[i]]) <- boot_dict[["ML"]]
  for(j in seq_along(1:length(dat_full_comp[[1]]))){
    names(dat_full_comp[[i]][[j]]) <- boot_dict[["ML"]]
  }
}
saveRDS(dat_full_comp, "boot_comparison_start.rds")

#So within each outcome type, the outer list is what's bootstrapped, the inner list is what's being taken as the reference
dat_full_comp <- readRDS("boot_comparison_start.rds")
dat_ref_comp <- readRDS("boot_data.rds")

comp_table_full <- lapply(c(1:4), function(.hold){
  
  #.hold_obs <- .hold+2
  
  comp_curr <- 
  lapply(which(unique(dat_full_comp[[.hold]][[1]][[2]]$Metric) %in% comparison_metrics), function(.metric){
    
    .metric <- unique(dat_full_comp[[.hold]][[1]][[2]]$Metric)[.metric]
    comp_table <- matrix(NA, ncol=length(boot_dict[["ML"]]), nrow=length(boot_dict[["ML"]]))
    colnames(comp_table) <- rownames(comp_table) <- boot_dict[["names"]]
    
    for(i in seq_len(length(boot_dict[["ML"]]))){
      for(j in seq_len(length(boot_dict[["ML"]]))){
        
        if(j > i){
          
          #Entry for the trajectory
          dat <- dat_full_comp[[.hold]][[i]][[j]][dat_full_comp[[.hold]][[i]][[j]]$time == "All",]
          diff_val <- sprintf("%.3f", round(dat$Diff[dat$Metric == .metric], 3))
          prop_val <- gsub(" ", "", dat$Prop_diff[dat$Metric == .metric])
          p_val <- gsub(" ", "", dat$Boot_p[dat$Metric == .metric])
          string_res_1 <- paste0(diff_val, "<br>(", prop_val, ")<br>", p_val)
          
          dat <- dat_full_comp[[.hold]][[j]][[i]][dat_full_comp[[.hold]][[j]][[i]]$time == "All",]
          diff_val <- sprintf("%.3f", round(dat$Diff[dat$Metric == .metric], 3))
          prop_val <- gsub(" ", "", dat$Prop_diff[dat$Metric == .metric])
          p_val <- gsub(" ", "", dat$Boot_p[dat$Metric == .metric])
          string_res_2 <- paste0(diff_val, "<br>(", prop_val, ")<br>", p_val)
          
          
          # #Entry for the observation
          # dat <- dat_full_comp[[.hold_obs]][[i]][[j]][dat_full_comp[[.hold_obs]][[i]][[j]]$time == "All",]
          # diff_val <- sprintf("%.3f", round(dat$Diff[dat$Metric == .metric], 3))
          # prop_val <- gsub(" ", "", dat$Prop_diff[dat$Metric == .metric])
          # p_val <- gsub(" ", "", dat$Boot_p[dat$Metric == .metric])
          # string_obs <- paste0(diff_val, "<br>(", prop_val, ")<br>", p_val)
           
          #Populate the matrix in comp_table
          comp_table[j,i] <- string_res_1; comp_table[i,j] <- string_res_2
        }
      }
    }
    return(comp_table)
  })
  names(comp_curr) <- unique(dat_full_comp[[.hold]][[1]][[2]]$Metric)[which(unique(dat_full_comp[[.hold]][[1]][[2]]$Metric) %in% comparison_metrics)]
  return(comp_curr)
})



saveRDS(comp_table_full, "boot_compare.rds")







update_dict <- list(ML = c("refmod", "merf", "glmertree_ref", "glmertree_bagged", "mvtboost", "fnn", "cnn", "lstm"),
                    dir = c("MERF/From Cheaha/_targets/objects/",
                            "MERF/From Cheaha/_targets/objects/",
                            "MERF_glmer/GLMER_ref/",
                            "MERF_glmer/Assembly/",
                            "MERF_mvt/From Cheaha/_targets/objects/",
                            rep("Neural Nets/_targets/objects/", 3)),
                    names = c("Reference", "MERF", "GLMER", "Bagged", "Boosted", "FNN", "CNN", "LSTM"))




comp_table_wobs <- lapply(c(1:2), function(.hold){
  
  .hold_obs <- .hold+2
  
  comp_curr <- 
    lapply(which(unique(dat_full_comp[[.hold]][[1]][[2]]$Metric) %in% comparison_metrics), function(.metric){
      
      .metric <- unique(dat_full_comp[[.hold]][[1]][[2]]$Metric)[.metric]
      comp_table <- matrix(NA, ncol=length(update_dict[["ML"]]), nrow=length(update_dict[["ML"]]))
      colnames(comp_table) <- rownames(comp_table) <- update_dict[["names"]]
      
      
      
      for(i in seq_len(length(update_dict[["ML"]]))){
       for(j in seq_len(length(update_dict[["ML"]]))){
          
         i_idx <- i; j_idx <- j
          
          if(j == i){
            ml_curr <- update_dict[["ML"]][i_idx]
            dir_curr <- paste0(dir_base, update_dict[["dir"]][i_idx])
            
            rds_eval <- paste0(ml_curr, "_", holdout_dict[["header"]][.hold], "_", "holdout_eval", holdout_dict[["obs"]][.hold])
            if(i==1 && holdout_dict[["obs"]][.hold] == "_obs") rds_eval <- paste0(rds_eval, "_noRE")
            eval_curr <- readRDS(paste0(dir_curr, rds_eval))
            eval_curr <- eval_curr[[1]]$All
            
            metric_curr <- eval_curr$Value[eval_curr$Metric == .metric]
            metric_curr <- sprintf("%.3f", round(metric_curr, 3))
            
            comp_table[i,j] <- paste0("<b>", metric_curr, "</b>")   #"<hr><style>hr{transform: rotate(20deg)}</style>"
          }
          
          if(j > i){
            
            #Entry for the trajectory
            if(i>1){
              dat <- dat_full_comp[[.hold]][[i_idx-1]][[j_idx-1]][dat_full_comp[[.hold]][[i_idx-1]][[j_idx-1]]$time == "All",]
            } else{
              dat <- dat_ref_comp[[j_idx-1]][[.hold]]$results[ dat_ref_comp[[j_idx-1]][[.hold]]$results$time == "All",]
            }
            diff_val <- sprintf("%.3f", round(dat$Diff[dat$Metric == .metric], 3))
            prop_val <- gsub(" ", "", dat$Prop_diff[dat$Metric == .metric])
            
            p_val <- dat$Boot_p[dat$Metric == .metric]
            if(!is.na(as.numeric(p_val)) && as.numeric(p_val) > 0.5){
              p_val <- 1-as.numeric(p_val)
              if(p_val == 0){ p_val <- "< 0.001"
              } else p_val <- as.character(p_val)
            }
            p_val <- gsub(" ", "", dat$Boot_p[dat$Metric == .metric])
            
            
            string_res_1 <- paste0(diff_val, "<br>(", prop_val, ")<br>", p_val)
            

            #Entry for the observation
            if(i>1){
              dat <- dat_full_comp[[.hold_obs]][[i_idx-1]][[j_idx-1]][dat_full_comp[[.hold_obs]][[i_idx-1]][[j_idx-1]]$time == "All",]
            } else{
              dat <- dat_ref_comp[[j_idx-1]][[.hold_obs]]$results[ dat_ref_comp[[j_idx-1]][[.hold_obs]]$results$time == "All",]
            }
            diff_val <- sprintf("%.3f", round(-1*dat$Diff[dat$Metric == .metric], 3))
            prop_val <- -1 * as.numeric(gsub(" %", "", dat$Prop_diff[dat$Metric == .metric]))
            prop_val <- paste0(prop_val, " %")
            #prop_val <- gsub(" ", "", dat$Prop_diff[dat$Metric == .metric])
            prop_val <- gsub(" ", "", prop_val)

            p_val <- dat$Boot_p[dat$Metric == .metric]
            if(!is.na(as.numeric(p_val)) && as.numeric(p_val) > 0.5){
              p_val <- 1-as.numeric(p_val)
              if(p_val == 0){ p_val <- "< 0.001"
              } else p_val <- as.character(p_val)
            }
            p_val <- gsub(" ", "", dat$Boot_p[dat$Metric == .metric])

            
            string_obs <- paste0(diff_val, "<br>(", prop_val, ")<br>", p_val)
            
            #Populate the matrix in comp_table
            comp_table[j,i] <- string_res_1; comp_table[i,j] <- string_obs
            
          }
        }
      }
      return(comp_table)
    })
  names(comp_curr) <- unique(dat_full_comp[[.hold]][[1]][[2]]$Metric)[which(unique(dat_full_comp[[.hold]][[1]][[2]]$Metric) %in% comparison_metrics)]
  return(comp_curr)
})

saveRDS(comp_table_wobs, "boot_compare_wobs.rds")


comp_table_wobs <- readRDS("boot_compare_wobs.rds")
comp_table_new <- 
lapply(comp_table_wobs, function(.xx){
  .out <-
  lapply(names(.xx), function(.name){
    
    for(i in seq_len(ncol(.xx[[.name]]))){
      for(j in seq_len(nrow(.xx[[.name]]))){
        if(j == i){
          .xx[[.name]][i,j] <- "<hr><style>hr{transform: rotate(45deg)}</style>"
        }
      }
    }
    return(.xx[[.name]])
  })
  names(.out) <- names(.xx)
  return(.out)
})



saveRDS(comp_table_new, "boot_compare_wobs.rds")









pagedown::chrome_print("E:/UAB SOPH/Dissertation/Defense/Defense-X2.html")
















#Portion to update the evaluation metrics


update_dict <- list(ML = c("refmod", "merf", "glmertree_ref", "glmertree_bagged", "mvtboost", "fnn", "cnn", "lstm"),
                  dir = c("MERF/From Cheaha/_targets/objects/",
                          "MERF/From Cheaha/_targets/objects/",
                          "MERF_glmer/GLMER_ref/",
                          "MERF_glmer/Assembly/",
                          "MERF_mvt/From Cheaha/_targets/objects/",
                          rep("Neural Nets/_targets/objects/", 3)),
                  names = c("Reference", "MERF", "GLMER", "Bagged", "Boosted", "FNN", "CNN", "LSTM"))

holdout_dict <- list(hold = c("adas", "cdr", "adas_obs", "cdr_obs"),
                     header = c("adas", "cdr", "adas", "cdr"),
                     obs = c("", "", "_obs", "_obs"),
                     outcome = c("ADASTOTAL11", "CDRSTAT", "ADASTOTAL11", "CDRSTAT"),
                     comp_ref = c(quote(ref_adas), quote(ref_cdr), quote(ref_adas_obs), quote(ref_cdr_obs)),
                     comp_boot = c(quote(ref_adas_boot), quote(ref_cdr_boot), quote(ref_adas_boot_obs), quote(ref_cdr_boot_obs)))


lapply(seq_along(1:length(update_dict[["ML"]])), function(.ml){
  
  #Prep the directory we're bootstrapping
  ml_curr <- update_dict[["ML"]][.ml]
  dir_curr <- paste0(dir_base, update_dict[["dir"]][.ml])
  print(ml_curr)
  
  
  
  #We iterate over each of the holdout types
  lapply(seq_along(1:length(holdout_dict[["hold"]])), function(.hold){
    
    print(holdout_dict[["hold"]][[.hold]])
    
    #Need to handle the noRE portion for reference forecasting
    obs_curr <- holdout_dict[["obs"]][.hold]
    if(ml_curr == "refmod" && obs_curr == "_obs") obs_curr <- paste0(obs_curr, "_noRE")
    if(ml_curr != "refmod" && obs_curr == "_obs"){
      obs_ref <- paste0(obs_curr, "_noRE")
    } else obs_ref <- paste0(obs_curr, "")
    
    #Build the argument sets and strings to pass to the boot function
    rds_curr <- paste0(ml_curr, "_", holdout_dict[["header"]][.hold], "_", "holdout_results", obs_curr)
    rds_eval <- paste0(ml_curr, "_", holdout_dict[["header"]][.hold], "_", "holdout_eval", obs_curr)
    
    results_curr <- readRDS(paste0(dir_curr, rds_curr))
    
    outcome <- holdout_dict[["outcome"]][.hold]
    
    if(ml_curr != "refmod"){
      rds_ref <- paste0("refmod", "_", holdout_dict[["header"]][.hold], "_", "holdout_results", obs_ref)
      ref_curr <- readRDS(paste0(dir_base, update_dict[["dir"]][1], rds_ref))
    }
    
    #Hijacking part of the boot function to update our results evaluations
    if(outcome == "CDRSTAT"){
      
        saveRDS(results_curr, paste0(dir_curr, rds_curr, "_pre"))
      
        new_thresh <- pROC::coords(pROC::roc(results_curr[[1]]$results$true, results_curr[[1]]$results$avg_score), "best")$threshold
        results_curr[[1]]$results$peak_mid <- new_thresh
        results_curr[[1]]$results$avg <- 0
        results_curr[[1]]$results$avg[results_curr[[1]]$results$avg_score > new_thresh] <- 1
        
        eval_curr <- readRDS(paste0(dir_curr, rds_eval))
        
        if(ml_curr != "refmod"){
          if(ml_curr %not_in% c("cnn", "lstm")){
            eval_update <- eval_ML_results(results_curr, outcome, cdr_ref = ref_curr)
          }else eval_update <- eval_ML_nn_results(results_curr, outcome, cdr_ref = ref_curr)
        } else eval_update <- eval_ML_results(results_curr, outcome)
        
        
        
        plot_update <- make_eval_plots(results_curr, outcome)
        plot_string <- paste0(ml_curr, "_", holdout_dict[["header"]][.hold], "_", "eval_plots", obs_curr)
        plot_curr <- readRDS(paste0(dir_curr, plot_string))
        
        saveRDS(eval_curr, paste0(dir_curr, rds_curr, "_pre"))
        saveRDS(plot_curr, paste0(dir_curr, plot_string, "_pre"))
        
        saveRDS(results_curr, paste0(dir_curr, rds_curr))
        saveRDS(eval_update, paste0(dir_curr, rds_eval))
        saveRDS(plot_update, paste0(dir_curr, plot_string))
        
    } else{
      eval_curr <- readRDS(paste0(dir_curr, rds_eval))
      eval_update <- eval_ML_results(results_curr, outcome)
      saveRDS(eval_curr, paste0(dir_curr, rds_eval, "_pre"))
      saveRDS(eval_update, paste0(dir_curr, rds_eval))
      
    }
      
      
      
     return(NULL)
  })
  
  
})

























#The updated version of the comaprison table builder, designed to include the reference and reordering

update_dict <- list(ML = c("refmod", "merf", "glmertree_ref", "glmertree_bagged", "mvtboost", "fnn", "cnn", "lstm"),
                    dir = c("MERF/From Cheaha/_targets/objects/",
                            "MERF/From Cheaha/_targets/objects/",
                            "MERF_glmer/GLMER_ref/",
                            "MERF_glmer/Assembly/",
                            "MERF_mvt/From Cheaha/_targets/objects/",
                            rep("Neural Nets/_targets/objects/", 3)),
                    names = c("Reference", "MERF", "GLMER", "Bagged", "Boosted", "FNN", "CNN", "LSTM"))




comp_table_wobs <- lapply(c(1:4), function(.hold){
  
  #.hold_obs <- .hold+2
  
  comp_curr <- 
    lapply(which(unique(dat_full_comp[[.hold]][[1]][[2]]$Metric) %in% comparison_metrics), function(.metric){
      
      .metric <- unique(dat_full_comp[[.hold]][[1]][[2]]$Metric)[.metric]
      comp_table <- matrix(NA, ncol=length(update_dict[["ML"]]), nrow=length(update_dict[["ML"]]))
      #colnames(comp_table) <- rownames(comp_table) <- update_dict[["names"]]
      
      
      #Function to set order for evaluation - first get the difference metrics
      diff_val_to_sort <- 
        sapply(seq_len(length(boot_dict[["ML"]])), function(.ml){
          if(.metric != "Bias"){
            diff_curr <- boot_data[[boot_dict[["ML"]][.ml]]][[holdout_dict$hold[.hold]]]$results
            diff_curr <- diff_curr$Diff[diff_curr$time == "All" & diff_curr$Metric == .metric]
          }else{
            rds_eval <- paste0(boot_dict[["ML"]][.ml], "_", holdout_dict[["header"]][.hold], "_", "holdout_eval", holdout_dict[["obs"]][.hold])
            dir_curr <- paste0(dir_base, update_dict[["dir"]][.ml+1])
            eval_curr <- readRDS(paste0(dir_curr, rds_eval))
            diff_curr <- abs(eval_curr[[1]]$All$Value[eval_curr[[1]]$All$Metric=="Bias"])
          }
          return(diff_curr)
        })
      #Then get the order and add 1 to the index (since we start with the reference)
      if(.metric %in% lower_is_better_eval){
        diff_val_to_sort <- order(diff_val_to_sort)
      } else diff_val_to_sort <- order(diff_val_to_sort, decreasing = TRUE)
      diff_val_to_sort <- c(1, diff_val_to_sort + 1)
      
      
      colnames(comp_table) <- rownames(comp_table) <- update_dict[["names"]][diff_val_to_sort]
      
      # for(i in seq_len(length(update_dict[["ML"]]))){
      #   for(j in seq_len(length(update_dict[["ML"]]))){
      for(i in seq_len(length(diff_val_to_sort))){
        for(j in seq_len(length(diff_val_to_sort))){
          
          i_idx <- diff_val_to_sort[i]; j_idx <- diff_val_to_sort[j]
          
          if(j == i){
            ml_curr <- update_dict[["ML"]][i_idx]
            dir_curr <- paste0(dir_base, update_dict[["dir"]][i_idx])
            
            rds_eval <- paste0(ml_curr, "_", holdout_dict[["header"]][.hold], "_", "holdout_eval", holdout_dict[["obs"]][.hold])
            if(i==1 && holdout_dict[["obs"]][.hold] == "_obs") rds_eval <- paste0(rds_eval, "_noRE")
            eval_curr <- readRDS(paste0(dir_curr, rds_eval))
            eval_curr <- eval_curr[[1]]$All
            
            metric_curr <- eval_curr$Value[eval_curr$Metric == .metric]
            if(.metric == "Bias") metric_curr <- metric_curr * -1
            metric_curr <- sprintf("%.3f", round(metric_curr, 3))
            
            comp_table[i,j] <- paste0("<br><b>", metric_curr, "</b><br><br>")   #"<hr><style>hr{transform: rotate(20deg)}</style>"
          }
          
          if(j > i){
            
            #Entry for the trajectory
            if(i>1){
              dat <- dat_full_comp[[.hold]][[i_idx-1]][[j_idx-1]][dat_full_comp[[.hold]][[i_idx-1]][[j_idx-1]]$time == "All",]
            } else{
              dat <- dat_ref_comp[[j_idx-1]][[.hold]]$results[ dat_ref_comp[[j_idx-1]][[.hold]]$results$time == "All",]
            }
            diff_val <- sprintf("%.3f", round(dat$Diff[dat$Metric == .metric], 3))
            prop_val <- gsub(" ", "", dat$Prop_diff[dat$Metric == .metric])
            
            p_val <- dat$Boot_p[dat$Metric == .metric]
            if(!is.na(as.numeric(p_val)) && as.numeric(p_val) > 0.5){
              p_val <- 1-as.numeric(p_val)
              if(p_val == 0){ p_val <- "< 0.001"
              } else p_val <- as.character(p_val)
            }
            p_val <- gsub(" ", "", dat$Boot_p[dat$Metric == .metric])
            
            
            string_res_1 <- paste0(diff_val, "<br>(", prop_val, ")<br>", p_val)
            

            #Entry for the CI
            ci_l <- sprintf("%.3f", round(dat$CI_l[dat$Metric == .metric],3))
            ci_u <- sprintf("%.3f",round(dat$CI_u[dat$Metric == .metric], 3))

            string_ci <- paste0("[", ci_l, ",<br>", ci_u, "]")
            
            #Populate the matrix in comp_table
            #comp_table[j,i] <- string_res_1; comp_table[i,j] <- string_obs
            comp_table[j,i] <- string_ci; comp_table[i,j] <- string_res_1
          }
        }
      }
      return(comp_table)
    })
  names(comp_curr) <- unique(dat_full_comp[[.hold]][[1]][[2]]$Metric)[which(unique(dat_full_comp[[.hold]][[1]][[2]]$Metric) %in% comparison_metrics)]
  return(comp_curr)
})

saveRDS(comp_table_wobs, "boot_compare_wobs.rds")











