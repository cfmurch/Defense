






boot_func <- function(dat = NULL, .file = NULL, ref_boot = NULL, true_ref = NULL, true_dat = NULL, .out = NULL, 
                      bootstraps = 1000, time_sets = c(0,0.25,0.5,0.75,1,1.5,2.5,3,4,5,6,"All"), return_res = TRUE, boot_only = FALSE){
  
  #Either read the RDS file or pass the list of results
  if(!is.null(.file) && is.null(dat)){
    list_curr <- readRDS(.file)
  } else list_curr <- dat
  
  #Get the results out of the list 
  dat_curr <- list_curr[[1]]$results
  
  bootstrap_data <-
    lapply(c(1:bootstraps), function(ii){
      
      #Tracker for sanity
      if(ii%%50==0) print(ii)
      
      #Sample from the predictions with replacement
      sample_boot <- sample(c(1:nrow(dat_curr)), nrow(dat_curr), replace = TRUE)
      
      #Get the bootstrapped results and slot it into a list that works with eval_ML_results
      dat_boot <- dat_curr[sort(sample_boot),]
      list_boot <- list_curr
      list_boot[[1]]$results <- dat_boot
      
      #Evaluate using the eval_ML_results function (which does require an outcome to be specified)
      eval_boot <- eval_ML_results(list_boot, .out)
      
      #Pass the list results into a dataframe and add an index for the bootstrap
      eval_curr <- rbind(eval_boot[[1]]$Group, eval_boot[[1]]$All)
      eval_curr <- data.frame(Boot = ii, eval_curr)
      
      #Cast to a wider dataframe to make comparisons easier
      eval_curr <- as.data.frame(tidyr::pivot_wider(eval_curr, id_cols = c("Boot", "time"), names_from = "Metric", values_from = "Value"))
      
      return(eval_curr)
    })
  
  #Bind the list and return if we're not doing a comparison
  bootstrap_data <- do.call(rbind.data.frame, bootstrap_data)
  if(boot_only == TRUE) return(bootstrap_data)
  
  
  #If reference has been passed we do comparisons - start by extracting the evaluation metrics we're comparing
  eval_cols <- colnames(bootstrap_data)[-which(colnames(bootstrap_data) %in% c("Boot", "time"))]
  
  #We then do the series of comparisons for each time point - currently not using the fixed time_sets would use `unique(bootstrap_data$time) %in% time_sets` if we were
  eval_comp <- lapply(unique(bootstrap_data$time), function(.time){
    #print(.time)
    #Within each time we evaluate each of the metrics in turn
    eval_comp_in_time <- lapply(eval_cols, function(.eval){
      #print(.eval)
      #First, get the true data value and the bootstrapped reference vector
      ref_eval_true <- true_ref$Value[true_ref$Metric == .eval & true_ref$time == .time]
      if(!is.null(ref_boot)) ref_eval_boot <- ref_boot[[.eval]][ref_boot$time == .time]
      boot_dat_curr <- bootstrap_data[[.eval]][bootstrap_data$time == .time]
      boot_dat_curr <- boot_dat_curr[!is.nan(boot_dat_curr)]
      boot_eval_true <- true_dat$Value[true_dat$Metric == .eval & true_dat$time == .time]
      
      
      #We have a quick branch in case we're not using boot strapping so no tests, this is essentially a truncated version of the usual path
      if(is.null(ref_boot)){
        
        #Same catch if the bootstrap metrics all have the same value
        if(length(unique(boot_dat_curr)) == 1 || length(boot_dat_curr) < 3){
          return(data.frame(Metric = .eval, Diff = NA, Prop_diff = NA, Boot_p = NA, CI_l = NA, CI_u = NA))
        }
        
        #BUilding a 95% CI
        ci_curr <- sort(boot_dat_curr - ref_eval_true)
        ci_u_idx <- floor(length(ci_curr)*0.975); ci_l_idx <- floor(length(ci_curr)*0.025)
        ci_l <- ci_curr[ci_l_idx]; ci_u <- ci_curr[ci_u_idx]
        
        
        #The same bias catch to create absolute values
        #We do this inside the eval catch below since we're calculating AV Bias directly as well
        # if(.eval == "Bias"){
        #   ref_eval_true <- abs(ref_eval_true); boot_dat_curr <- abs(boot_dat_curr); boot_eval_true <- abs(boot_eval_true)
        # }
        
        #Get the boot p-value based on the high/low toggle
        if(.eval %in% lower_is_better_eval){
          if(.eval != "Bias"){
            pval_curr <- sum(boot_dat_curr >= ref_eval_true) / length(boot_dat_curr)
          } else{
            pval_curr <- sum(abs(boot_dat_curr) >= abs(ref_eval_true)) / length(boot_dat_curr)
          }
        } else{
          pval_curr <- sum(boot_dat_curr <= ref_eval_true) / length(boot_dat_curr)
        }
          
        #Calculate the percent difference relative to the reference
        prop_diff <- (boot_eval_true - ref_eval_true)/ref_eval_true * 100
        diff_curr <- boot_eval_true - ref_eval_true
        
        #Calculate and format the p-value and proportional difference
        if(as.numeric(pval_curr) == 0) {pval_curr <- "< 0.001"
        } else pval_curr <- sprintf("%.3f", pval_curr)
        prop_diff <- paste0(sprintf("%.2f", prop_diff), " %")
        
        return(data.frame(Metric = .eval, Diff = diff_curr, Prop_diff = prop_diff, Boot_p = as.character(pval_curr), CI_l = ci_l, CI_u = ci_u))
      }
      
      #Otherwise we're doing some evaluation tests and can call the appropriate bootstrapped results (only used for refmod but not actually utilized)
      
      #A catch in case all metrics are the same value (e.g. precision of 1)
      if((length(unique(ref_eval_boot)) == 1 && length(unique(boot_dat_curr)) == 1) || 
         length(ref_eval_boot) < 3 || length(boot_dat_curr) < 3){
        return(data.frame(Metric = .eval, Diff = NA, Prop_diff = NA, Boot_p = NA, Test_stat = NA, Test_p = NA, CI_l = NA, CI_u = NA))
      }
      
      #BUilding a 95% CI
      ci_curr <- sort(boot_dat_curr - ref_eval_true)
      ci_u_idx <- floor(length(ci_curr)*0.975); ci_l_idx <- floor(length(ci_curr)*0.025)
      ci_l <- ci_curr[ci_l_idx]; ci_u <- ci_curr[ci_u_idx]
      
      
      #If the metric is not Bias we do a standard comparison with a t.test
      if(.eval != "AV Bias"){
        test_curr <- "t.test"
        pval_curr <- sprintf("%.3f", sum(boot_dat_curr >= ref_eval_true) / length(boot_dat_curr))
        
        
        #Otherwise if it's bias we use the absolute value and a wilcox test  
      } else{
        #ref_eval_true <- abs(ref_eval_true); ref_eval_boot <- abs(ref_eval_boot); boot_dat_curr <- abs(boot_dat_curr); boot_eval_true <- abs(boot_eval_true)
        test_curr <- "wilcox.test"
      }
      
      #Get the boot p-value based on the high/low toggle
      if(.eval %in% lower_is_better_eval){
        if(.eval != "Bias"){
          pval_curr <- sum(boot_dat_curr >= ref_eval_true) / length(boot_dat_curr)
          prop_diff <- (boot_eval_true - ref_eval_true)/ref_eval_true * 100
        } else{
          pval_curr <- sum(abs(boot_dat_curr) >= abs(ref_eval_true)) / length(boot_dat_curr)
          prop_diff <- (abs(boot_eval_true) - abs(ref_eval_true))/abs(ref_eval_true) * 100
        }
      } else{
        pval_curr <- sum(boot_dat_curr <= ref_eval_true) / length(boot_dat_curr)
        prop_diff <- (ref_eval_true - boot_eval_true)/ref_eval_true * 100
      }
      
      
      #Evaluate the test (this is probalby not going to be used in practice, at least not comparing the reference)
      if(!(length(table(boot_dat_curr))==1 && length(table(ref_eval_boot))==1)){
        test_curr <- do.call(test_curr, args = list(x = boot_dat_curr, y = ref_eval_boot))
        test_stat <- round(test_curr$statistic, 3)
        test_pval <- test_curr$p.value
      } else{
        test_stat <- NA; test_pval <- NA
      }
      
      #Calculate the percent difference relative to the reference
      if(.eval != "Bias"){
        prop_diff <- (boot_eval_true - ref_eval_true)/ref_eval_true * 100
      } else (abs(boot_eval_true) - abs(ref_eval_true))/abs(ref_eval_true) * 100
      diff_curr <- boot_eval_true - ref_eval_true
      
      #Calculate and format the p-value and proportional difference
      if(as.numeric(pval_curr) == 0) {pval_curr <- "< 0.001"
      } else pval_curr <- sprintf("%.3f", pval_curr)
      prop_diff <- paste0(sprintf("%.2f", prop_diff), " %")
      
      #Format the p-value from the t/wilcox test
      if(!is.na(test_pval)){
        if(test_pval < 0.001){ test_pval <- "< 0.001"
        } else test_pval <- sprintf("%.3f", test_pval)
      }
      
      return(data.frame(Metric = .eval, Diff = diff_curr, Prop_diff = prop_diff, Boot_p = as.character(pval_curr), Test_stat = test_stat, Test_p = as.character(test_pval), CI_l = ci_l, CI_u = ci_u))
      
      
    })
    
    return(data.frame(time = .time, do.call(rbind, eval_comp_in_time)))
    
  })
  
  eval_comp <- do.call(rbind, eval_comp)
  rownames(eval_comp) <- NULL
  
  if(return_res == TRUE){
    return(list(boot_data = bootstrap_data, results = eval_comp))  
  } else{
    return(eval_comp)
  }
  
  
}











#Companion function for building confidence intervals, specifically used in Aim 3
#Is a reduced version of the RandEff Impute version to be called by the table builder

ci_boot_func_impute <- function(dat1, dat2, .bias, .boot = 1000){
  
  boot_res <- rep(NA, .boot)
  
  for(i in 1:.boot){
    
    dat1_bs <- sample(dat1, floor(length(dat1)*0.2), replace = TRUE)
    dat2_bs <- sample(dat2, floor(length(dat2)*0.2), replace = TRUE)
    
    if(.bias == TRUE){
      diff_bs <- (median(abs(dat2_bs))-median(abs(dat1_bs)))/min(median(abs(dat1_bs)), median(abs(dat2_bs)))
    } else{
      diff_bs <- (mean(dat2_bs)-mean(dat1_bs))/min(mean(dat1_bs), mean(dat2_bs))
    }
    
    
    boot_res[i] <- diff_bs
    
  }
  
  boot_res <- sort(boot_res)
  test_p <- c(boot_res[25], boot_res[975])
  
  test_p <- paste0(paste(sprintf("%.1f", test_p*100), collapse = "% -<br>"), "%")
  
  return(test_p)
  
}








