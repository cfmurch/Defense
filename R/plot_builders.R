#'
#' Functions to rebuild plots from the ggplot data
#'





plot_diff_adas_rebuild <- function(.dat){
  
  group_var <- "time"
  outcome <- "ADASTOTAL11"
  
  if(length(.dat[["diff"]][.dat[[group_var]] == 2.5]) == 1) .dat <- .dat[-which(.dat[[group_var]]==2.5),]
                                                                          
  plot_curr <- 
    ggplot(data=.dat, aes_string(x = "time", y = "diff")) + 
    geom_point(aes(color = misclass), position = position_jitter(width=0.15), size = 2) + 
    geom_hline(yintercept = 0, color = "red") + 
    geom_errorbar(stat = "summary", fun.data = "mean_sdl", fun.args = list(mult=1), width = 0.2, size = 1.25, alpha = 0.45) + 
    stat_summary(fun = mean, geom = "point", shape = 45, size = 18) +
    
    scale_x_continuous(name = "Years", breaks = c(0:100), labels=c(0:100)) + 
    scale_y_continuous(name = paste0("ADAS-Cog:", " Predict - Actual")) +
    scale_color_brewer(type = "qual", palette = "Set1", guide = guide_legend(override.aes = list(size=4, fill=NA))) +
    expand_limits(x = c(min(.dat[[group_var]], max(.dat[[group_var]])))) + 
    
    theme_explorer
  
  return(plot_curr)
}

plot_pred_adas_rebuild <- function(.dat){
  
  outcome <- "ADASTOTAL11"
  group_var <- "time"
  
  if(length(.dat[[outcome]][.dat[[group_var]] == 2.5]) == 2) .dat <- .dat[-which(.dat[[group_var]]==2.5),]
  
  plot_curr <- 
    ggplot(data = .dat, aes_string(x = group_var, y = outcome)) + 
    geom_point(aes(color = Type), position = position_jitterdodge(dodge.width=0.20), size=2) +
    geom_errorbar(aes(group = Type), stat = "summary", fun.data = "mean_sdl", fun.args = list(mult = 1), position = position_dodge(width=0.20), width = 0.2, size = 1.25, alpha = 0.45) + 
    stat_summary(aes(group = Type), fun = mean, geom = "point", shape = 45, size = 18, position = position_dodge(width=0.20)) + 
    
    scale_color_manual(values = RColorBrewer::brewer.pal(3, "Dark2")[1:2], labels=c("Actual", "Predicted"), guide = guide_legend(override.aes = list(size=4, fill=NA))) + 
    scale_x_continuous(name = "Years", breaks = c(0:100), labels=c(0:100)) + 
    scale_y_continuous(name = "ADAS-Cog 11") + 
    expand_limits(x = c(min(.dat[[group_var]], max(.dat[[group_var]])))) + 
    
    theme_explorer
  
  return(plot_curr)
  
  
}



plot_diff_cdr_rebuild <- function(.dat, allrows){
  group_var <- "time"
  #allrows <- nrow(allrows)
  
  .dat_orig <- .dat
  .dat <- 
  lapply(sort(unique(.dat$time)), function(xx){
    .out <- rbind(.dat[.dat$time==xx & .dat$misclass == "False Positive",][1,], .dat[.dat$time==xx & .dat$misclass == "False Negative",][1,])
    .out$prop <- NA
    .out$prop[1] <- nrow(.dat[.dat$time==xx & .dat$misclass == "False Positive",]) / nrow(allrows[allrows$time==xx,])
    .out$prop[2] <- nrow(.dat[.dat$time==xx & .dat$misclass == "False Negative",]) / nrow(allrows[allrows$time==xx,])
    return(.out)
  })
  .dat <- do.call(rbind, .dat)
  .dat <- .dat[-which(is.na(.dat$time)),]
  
  plot_curr <- 
    ggplot(data = .dat[!.dat$misclass == "Correct",], aes_string(x = group_var)) + 
    #geom_bar(aes(y=(..count..)/(sum(..count..) + (allrows - nrow(.dat))), fill = misclass), position = position_dodge(0.40), width = 0.35, color = "black") + 
    geom_bar(aes(y=prop, fill = misclass), stat="identity", position = position_dodge(0.40), width = 0.35, color = "black") + 
    
    scale_fill_brewer(breaks = c("False Positive", "False Negative"), type = "qual", palette = "Set1") + 
    scale_y_continuous(name = "Proportion misclassified by time", labels = scales::percent_format()) + 
    scale_x_continuous(name = "Years", breaks=c(0:100), labels=c(0:100)) + 
    expand_limits(x = c(min(.dat[[group_var]], max(.dat[[group_var]])))) + 
    
    theme_explorer
  
  return(plot_curr)
}


plot_pred_cdr_rebuild <- function(.dat){
  group_var <- "time"
  outcome <- "CDRSTAT"
  
  plot_curr <- 
    ggplot(data = .dat[.dat[[outcome]]!=0,], aes_string(x = group_var,)) + 
    geom_bar(aes(fill = Type), position = position_dodge(0.40), width = 0.35, color = "black") + 
    
    scale_fill_manual(values = RColorBrewer::brewer.pal(3, "Dark2")[1:2], labels=c("Actual", "Predicted"), drop = FALSE) + 
    scale_y_continuous(name = "N Impaired (CDR 0.5+)") + 
    scale_x_continuous(name = "Years", breaks=c(0:100), labels=c(0:100)) +
    expand_limits(x = c(min(.dat[[group_var]], max(.dat[[group_var]])))) + 
    
    theme_explorer
  
  return(plot_curr)
}



plot_hyper_rebuild <- function(.dat, size_shift = TRUE, shift_val = 3){
  group_var = "hyper"
  outcome = "Value"
  
  .dat$Metric <- gsub("Symm MAE Percent", "Symm MAE\\\nPercent", .dat$Metric)
  .dat$Metric <- gsub("Class Error", "Class\\\nError", .dat$Metric)
  
  if(length(grep("layer_count", .dat$hyper))>0){
    # levels_temp <- as.character(levels(.dat$hyper))
    # levels_temp <- gsub("(dense|cnn|lstm)_[1-3]_(activ|kern_constraint|bias_constraint|pool_size|kernel_size)(.*?)(;|NULL|relu)", "", levels_temp)
    # levels_temp <- gsub(";\\s*;", ";", levels_temp)
    # .dat$hyper <- factor(.dat$hyper, levels = levels(.dat$hyper), labels=levels_temp)
    
    .dat$hyper <- gsub("(dense|cnn|lstm)_[1-3]_(activ|kern_constraint|bias_constraint|pool_size|kernel_size)(.*?)(;|NULL|relu)", "", .dat$hyper)
    .dat$hyper <- gsub("layer_count\\s*=\\s*\\d\\s*;\\s*", "", .dat$hyper)
    .dat$hyper <- gsub(";\\s*;", ";", .dat$hyper)
    .dat$hyper <- gsub("\\s*;\\s*;\\s*", "\\\n", .dat$hyper)
    #.dat$hyper <- gsub(";\\s*cnn_2", "\\\ncnn_2", .dat$hyper)
    .dat$hyper <- gsub(";\\s*cnn_2.*", "", .dat$hyper)
    .dat$hyper <- gsub("\\\n\\s*lstm_2.*", "", .dat$hyper)
    .dat$hyper <- gsub(";\\s*dense_3", "\\\ndense_3", .dat$hyper)
    .dat$hyper <- gsub("\\s*;\\s*", "", .dat$hyper)
    
  
  }else {
    if(nchar(as.character(.dat$hyper[1])) > 35){
      # levels_temp <- as.character(levels(.dat$hyper))
      # levels_temp <- gsub("; n_", "\\\nn_", levels_temp)
      # .dat$hyper <- factor(.dat$hyper, levels=levels(.dat$hyper), labels=levels_temp)
      
      .dat$hyper <- gsub("; n_", "\\\nn_", .dat$hyper)
      
    }
  }
  
  plot_curr <- 
    ggplot(data = .dat) + 
    geom_bar(aes(x = Metric, y = Value, fill = hyper), stat = "identity", position=position_dodge(), color="black") + 
    theme_hyper
  
  if(size_shift == TRUE){
    plot_curr <- plot_curr + ggplot2::theme(legend.key = ggplot2::element_rect(fill=NA, color=NA), legend.key.size = ggplot2::unit(shift_val, "lines"))
  }
  
  return(plot_curr)
}









