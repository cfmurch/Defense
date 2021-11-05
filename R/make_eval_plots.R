#'
#'
#' Generic plotting function
#'
#'


make_eval_plots <- function(results_list, outcome,
                                group_var = "time", id_var = "id",
                                var_actual = "true", var_pred = "avg",
                                metric_dict = metrics_data_dict){
  
  plots_eval <- lapply(results_list, function(.hyper){
    
    #Extract the results df
    df <- .hyper[["results"]]
    
    #Calculate the difference (raw value for continuous, misclassification for categorical)
    df$diff <- df[[var_pred]] - df[[var_actual]]
    
    #Make the long df
    df_plot_actual <- data.frame(df[,colnames(df) %in% c(group_var, var_actual)], Type = 0)
    df_plot_pred <- data.frame(df[,colnames(df) %in% c(group_var, var_pred)], Type = 1)
    colnames(df_plot_pred)[which(colnames(df_plot_pred) == var_pred)] <- var_actual
    
    #Bind the rows
    df_plot <- rbind(df_plot_actual, df_plot_pred)
    df_plot$Type <- factor(df_plot$Type)
    colnames(df_plot)[colnames(df_plot) == var_actual] <- outcome
    
    #Regression plot
    if(outcome == "ADASTOTAL11"){
      
      df$misclass <- "Overestimated"
      df$misclass[df$diff < 0] <- "Underestimated"
      df$misclass <- factor(df$misclass)
      
      
      plot_diff <- 
        ggplot(data=df, aes_string(x = group_var, y = "diff")) + 
        geom_point(aes(color = misclass), position = position_jitter(width=0.15), size = 2) + 
        geom_hline(yintercept = 0, color = "red") + 
        geom_errorbar(stat = "summary", fun.data = "mean_sdl", fun.args = list(mult=1), width = 0.2, size = 1.25, alpha = 0.45) + 
        stat_summary(fun = mean, geom = "point", shape = 45, size = 18) +
        
        scale_x_continuous(name = group_var, breaks = c(0:100), labels=c(0:100)) + 
        scale_y_continuous(name = paste0(outcome, " - Pred - Actual")) +
        scale_color_brewer(type = "qual", palette = "Set1") +
        
        theme_explorer
      
      plot_curr <- 
        ggplot(data = df_plot, aes_string(x = group_var, y = outcome)) + 
        geom_point(aes(color = Type), position = position_jitterdodge(dodge.width=0.20), size=2) +
        geom_errorbar(aes(group = Type), stat = "summary", fun.data = "mean_sdl", fun.args = list(mult = 1), position = position_dodge(width=0.20), width = 0.2, size = 1.25, alpha = 0.45) + 
        stat_summary(aes(group = Type), fun = mean, geom = "point", shape = 45, size = 18, position = position_dodge(width=0.20)) + 
        
        scale_color_manual(values = RColorBrewer::brewer.pal(3, "Dark2")[1:2], labels=c("Actual", "Predicted")) + 
        scale_x_continuous(name = group_var, breaks = c(0:100), labels=c(0:100)) + 
        
        theme_explorer
      
      #Classification plot
    } else{
      
      #Build the misclassifications data
      misclass_labs <- c("Correct", "False Positive", "False Negative")
      df$misclass <- as.character(rep("Correct", nrow(df)))
      df$misclass[df$diff > 0] <- "False Positive"  
      df$misclass[df$diff < 0] <- "False Negative"
      df$misclass <- factor(df$misclass, levels = misclass_labs, labels = misclass_labs)
      
      plot_diff <- 
        ggplot(data = df[!df$misclass == "Correct",], aes_string(x = group_var)) + 
        geom_bar(aes(y=(..count..)/sum(..count..), fill = misclass), position = position_dodge(0.40), width = 0.35, color = "black") + 
        
        scale_fill_brewer(breaks = c("False Positive", "False Negative"), type = "qual", palette = "Set1") + 
        scale_y_continuous(name = "Proportion misclassified", labels = scales::percent_format()) + 
        scale_x_continuous(breaks=c(0:100), labels=c(0:100)) + 
        
        theme_explorer
      print(plot_diff)
      
      plot_curr <- 
        ggplot(data = df_plot[df_plot[[outcome]]!=0,], aes_string(x = group_var,)) + 
        geom_bar(aes(fill = Type), position = position_dodge(0.40), width = 0.35, color = "black") + 
        
        scale_fill_manual(values = RColorBrewer::brewer.pal(3, "Dark2")[1:2], labels=c("Actual", "Predicted"), drop = FALSE) + 
        scale_x_continuous(breaks=c(0:100), labels=c(0:100)) + 
        
        theme_explorer
      
    }
    return(list(plot = plot_curr, diff = plot_diff, hyperparams = .hyper[["hyperparams"]]))
    
  })
  
  hyper_names <- lapply(plots_eval, function(.hyper){.hyper[["hyperparams"]]})
  hyper_names <- do.call(c, hyper_names)
  names(plots_eval) <- hyper_names
  
  return(plots_eval)
  
  
}































#The old function that didn't consider hyperparameters

make_eval_plots_old <- function(df, outcome, group_var = "time", id_var = "id",
                            var_actual = "true", var_pred = "avg",
                            metric_dict = metrics_data_dict){
  
  
  plots_eval <- lapply(outcome, function(.resp){
    
    #Make the long df
    df_plot_actual <- data.frame(df[,colnames(df) %in% c(group_var, var_actual)], Type = 0)
    df_plot_pred <- data.frame(df[,colnames(df) %in% c(group_var, var_pred)], Type = 1)
    colnames(df_plot_pred)[which(colnames(df_plot_pred) == var_pred)] <- var_actual
    
    #Bind the rows
    df_plot <- rbind(df_plot_actual, df_plot_pred)
    df_plot$Type <- factor(df_plot$Type)
    colnames(df_plot)[colnames(df_plot) == var_actual] <- .resp
    
    #Regression plot
    if(outcome == "ADASTOTAL11"){
      plot_curr <- 
        ggplot(data = df_plot, aes_string(x = group_var, y = .resp)) + 
        geom_point(aes(color = Type), position = position_jitterdodge(dodge.width=0.5), size=2) +
        geom_errorbar(aes(group = Type), stat = "summary", fun.data = "mean_sdl", fun.args = list(mult = 1), position = position_dodge(width=0.5), width = 0.2, size = 1.25, alpha = 0.45) + 
        stat_summary(aes(group = Type), fun = mean, geom = "point", shape = 45, size = 18, position = position_dodge(width=0.5)) + 
        
        scale_color_manual(values = RColorBrewer::brewer.pal(3, "Dark2")[1:2], labels=c("Actual", "Predicted")) + 
        scale_x_continuous(name = group_var) + 
        
        theme_explorer
      
      #Classification plot
    } else{
      plot_curr <- 
        ggplot(data = df_plot[df_plot[[.resp]]!=0,], aes_string(x = group_var,)) + 
        geom_bar(aes(fill = Type), position = position_dodge(0.5), color = "black") + 
        
        scale_fill_manual(values = RColorBrewer::brewer.pal(3, "Dark2")[1:2], labels=c("Actual", "Predicted"), drop = FALSE) + 
        
        theme_explorer
      
    }
    return(plot_curr)
    
  })
  
  return(plots_eval)
  
  
}
