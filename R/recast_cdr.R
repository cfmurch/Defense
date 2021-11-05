




# 
# cdr_prop_func <- function(){
#   
#   dir_base <- "E:/UAB SOPH/Dissertation/Full Project - Meta Database/"
#   dat <- readRDS(paste0(dir_base, "MERF/_targets/objects/holdout_split_dat"))
#   dat <- dat$all$retained
#   
#   cdr_prop <- lapply(sort(unique(dat$VISTIME)), function(.time){
#     data.frame(VISTIME = .time, CDR_prop = (1-mean(na.omit(dat$CDRSTAT[dat$VISTIME == .time]))))
#   })
#   
#   cdr_prop <- do.call(rbind, cdr_prop)
#   cdr_prop <- cdr_prop[!is.nan(cdr_prop$CDR_prop),]
#   rownames(cdr_prop) <- NULL
#   
#   return(cdr_prop)
# }
#   
# 
# 
# 
# re_eval_cdr <- function(.ml){
#   
#   ml_curr <- boot_dict[["ML"]][.ml]
#   ml_dir <- paste0(dir_base, boot_dict[["dir"]][.ml])
#   
#   ml_res_str <- paste0(ml_dir, ml_curr, "_cdr_holdout_results")
#   ml_res <- readRDS(ml_res_str)
#   ml_res[[1]]$results$avg_old <-  ml_res[[1]]$results$avg
#   ml_res[[1]]$results$avg <- 0
#   
#   for(.time in cdr_dict$VISTIME){
#     ml_res[[1]]$results$avg[ml_res[[1]]$results$time == .time & ml_res[[1]]$results$avg_score > cdr_dict$CDR_prop[cdr_dict$VISTIME == .time]] <- 1
#   }
#   
#   saveRDS(ml_res, ml_res_str)
#   
#   ml_obs_str <- paste0(ml_dir, ml_curr, "_cdr_holdout_results_obs")
#   ml_obs <- readRDS(ml_obs_str)
#   ml_res[[1]]$results$avg_old <-  ml_res[[1]]$results$avg
#   
#   
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# invisible(
# lapply(seq_len(length(boot_dict[["ML"]])), function(.ml){
#   
#   ml_curr <- boot_dict[["ML"]][.ml]
#   ml_dir <- paste0(dir_base, boot_dict[["dir"]][.ml])
#   
#   ml_res <- readRDS(paste0(ml_dir, ml_curr, "_cdr_holdout_results_obs"))
#  print(paste0(ml_curr, " - ", ml_res[[1]]$results$peak_mid[1]))
#   return(NULL)
#   
# })
# 
# )
# 
