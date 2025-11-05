library(brms)
library(dplyr)

## funcs ##
custom_credible_intervals <- function(model, newdata) {
  # Extract posterior predictive samples without summarizing
  pred_samples <- predict(model, newdata = newdata, allow_new_levels = TRUE, summary = FALSE)
  
  # Compute custom credible intervals (e.g., 5%, 25%, 50%, 75%, 95%)
  custom_intervals <- apply(pred_samples, 2, quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  
  # Convert to a data frame for easier interpretation
  custom_intervals_df <- as.data.frame(t(custom_intervals))
  colnames(custom_intervals_df) <- c("low5", "low25", "median", "high75", "high95")
  
  # View the result
  # head(custom_intervals_df)
  return(custom_intervals_df)
}


## data ##
# read in pre-computed phylo_df etc. 
# phylo_df <- read.csv('../data/isca/phylo_df.csv') # this is just the species we have full or near full data for
# phylo_df <- phylo_df %>% filter(dataset_id != 'Zimbabwe(matt)')
phylo_df_all <- read.csv('../data/isca/phylo_df_all_v4.csv') # this is all species
phylo_df_all <- phylo_df_all %>% filter(location != 'Zimbabwe(matt)')
# Set invasive_status levels so that "native" is the reference category
# phylo_df_all_test <- phylo_df_all %>% mutate(invasive_status = ifelse(dataset_id=="IUCN", "native", invasive_status))
phylo_df_all$invasive_status <- as.factor(phylo_df_all$invasive_status)
phylo_df_all$invasive_status <- relevel(phylo_df_all$invasive_status, ref = "native")
# vcov <- read.csv("../data/isca/spp_level_cov_matrix.csv")
vcov <- readRDS('../data/vcov_v4.RDS')

m_gpt = brm(correct_or_not_gpt.mean | mi() ~ 1 + 
            scale(body_log) + scale(range_log) + 
            iucn_category + 
            (1|sci_name) + (1|location) + 
            (1|gr(tips_chr, cov = A)), 
            data=phylo_df_all, 
            data2= list(A=vcov), 
            iter=8000, 
            warmup=5000, 
            chains = 6, 
            cores = 6, 
            family = Beta# ,
            # control = list(adapt_delta = 0.99, max_treedepth = 15) 
            )

# m_gpt = brm(correct_or_not_gpt.mean | mi() ~ 1 + 
#             scale(body_log) + scale(range_log) + 
#             iucn_category + invasive_status + 
#             (1|sci_name) + (1|dataset_id) + 
#             (1|gr(tips_chr, cov = A)), 
#             data=phylo_df_all, 
#             data2= list(A=vcov), 
#             iter=8000, 
#             warmup=5000, 
#             chains = 6, 
#             cores = 6, 
#             family = Beta# ,
#             # control = list(adapt_delta = 0.99, max_treedepth = 15) 
#             )
saveRDS(m_gpt, file="../data/isca/m_gpt_isca_v9_native_ref.RDS")