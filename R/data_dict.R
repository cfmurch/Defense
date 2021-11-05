#'
#' Usual data dictionary
#'
#'


#Varaibles used for ML methods
model_var_dict <- list(yvar_cont = "ADASTOTAL11",
                       yvar_cat = "CDRSTAT",
                       yvar = c("ADASTOTAL11", "CDRSTAT"),
                       xvar = c("AGE", "SEX", "APOE", "RACE", "ETHNIC", "EDUCFAC", "MMSCORE", "ADMED", "BPDIA", "BPSYS", "WEIGHT"),
                       xvar_ref = c("AGE", "SEX", "APOE", "MMSCOREBL"),
                       time_xvar_itx = c("MMSCORE"),
                       id_var = "UID",
                       time_var = "VISTIME")




#Data dictionary for metrics to call

metrics_data_dict <- list(class = list(name = c("Accuracy", "F1 Score", "Class Error", "Precision", "Recall", "AUC", "NRI"),
                                       func = c("accuracy", "f1", "ce", "precision", "recall", "auc", "nri_binom"),
                                       avg = c("mean", "mean", "mean", "mean", "mean", "mean", "mean"),
                                       qual = c("max", "max", "min", "max", "max", "max", "max"),
                                       abs = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)),
                          reg = list(name = c("RMSE", "Median AE", "Symm MAE Percent", "Bias", "AV Bias"),
                                     func = c("rmse", "mae", "smape", "bias", "bias"),
                                     avg = c("mean", "median", "median", "median", "median"),
                                     qual = c("min", "min", "min", "min", "min"),
                                     abs  = c(FALSE, FALSE, FALSE, TRUE, TRUE))
)
metrics_data_dict[["class"]] <- 
  lapply(metrics_data_dict[["class"]], function(xx){
    xx[-which(metrics_data_dict[["class"]][["name"]] == "F1 Score")]})



#Data dictionary for plotting purposes
plot_data_dict <- list(real_var = c("ADASTOTAL11", "CDRSTAT"),
                       syn_var = c("ADAS_norm_syn", "CDRSTAT_syn"),
                       y_axis = c("ADAS Cog", "CDR Impaired"))



#Some commonly used variables for LMM formulation and other processing
time_var <- "VISTIME"; clust_var <- "cluster"
id_var <- "UID"; syn_id_var <- "SID"; link_id_var <- "link_id"


#Number of clusters used in `cluster_participants` and LMM parameterization
number_of_clusters <- 10

#glmmTMB families - just used beta regression
tmb_fam <- c("beta_family")



#Usual helpers
`%not_in%` <- Negate(`%in%`)
not_na <- Negate(is.na)
not_null <- Negate(is.null)
expit <- function(x){exp(x)/(1+exp(x))}


theme_explorer <- 
  ggplot2::theme_bw() + 
  ggplot2::theme(legend.title=ggplot2::element_blank(), legend.background=ggplot2::element_blank()) + 
  ggplot2::theme(plot.margin = ggplot2::unit(c(1.2,0.1,0,0), "lines")) + 
  ggplot2::theme(legend.position=c(0.5,1.03), legend.direction="horizontal", legend.margin=ggplot2::margin(c(0,0,0,0), unit='in')) + 
  ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5, size=24)) +
  ggplot2::theme(axis.title=ggplot2::element_text(size=18), axis.text=ggplot2::element_text(size=14, color="black")) + 
  ggplot2::theme(legend.text=ggplot2::element_text(size=16))

theme_manus <- 
  ggplot2::theme_classic() + 
  ggplot2::theme(legend.title=ggplot2::element_blank(), legend.background=ggplot2::element_blank()) + 
  ggplot2::theme(plot.margin = ggplot2::unit(c(1.2,0,0,0), "lines")) + 
  ggplot2::theme(legend.position=c(0.5,1.03), legend.direction="horizontal", legend.margin=ggplot2::margin(c(0,0,0,0), unit='in')) + 
  ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5, size=24)) +
  ggplot2::theme(axis.title=ggplot2::element_text(size=18), axis.text=ggplot2::element_text(size=14, color="black")) + 
  ggplot2::theme(legend.text=ggplot2::element_text(size=16))
  
  

theme_hyper <- 
  ggplot2::theme_bw() + 
  ggplot2::theme(legend.title=ggplot2::element_blank(), legend.background=ggplot2::element_blank()) + 
  #ggplot2::theme(legend.key = ggplot2::element_rect(fill=NA, color=NA), legend.key.size = ggplot2::unit(3, "lines")) + 
  #ggplot2::theme(plot.margin = ggplot2::unit(c(1.2,0.1,0,0), "lines")) + 
  ggplot2::theme(legend.position="right", legend.direction="vertical", legend.margin=ggplot2::margin(c(0,0,0,0), unit='in')) + 
  ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5, size=24)) +
  ggplot2::theme(axis.title=ggplot2::element_text(size=18), axis.text=ggplot2::element_text(size=14, color="black")) + 
  ggplot2::theme(legend.text=ggplot2::element_text(size=16))



draw_key_orig <- ggplot2::GeomBar$draw_key

draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)
  
  grid::rectGrob(
    width = grid::unit(0.6, "npc"),
    height = grid::unit(0.6, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    ))
}



#withCallingHandlers function to return both evaluated expression and warnings

withWarnings <- function(expr) {
  myWarnings <- NULL
  
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, list(w))
    invokeRestart("muffleWarning")
  }
  
  val <- withCallingHandlers(expr, warning = wHandler)
  
  return(list(value = val, warnings = myWarnings))
} 





