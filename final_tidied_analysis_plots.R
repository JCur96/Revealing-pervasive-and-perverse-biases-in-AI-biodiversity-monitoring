# Tidied final code for plotting / analysis
##### Imports #####
library(readxl)
library(rotl)
library(ape)
library(tidyr)
library(dplyr)
library(brms)
library(ggplot2)
library(ggforce)
library(ggridges)
library(ggtree)
library(ggnewscale)
library(patchwork)
library(cowplot)
library(marginaleffects)
library(ggpubr)
##### Functions #####
#Functions to extract marginal/partial/conditional effects
marg_sum = function(model, variable, samples = 1000){
  m_df = conditional_effects(model, variable, ndraws = samples, method = "posterior_epred")
  pred_mat = posterior_epred(model, newdata = m_df[[1]], ndraws = samples, allow_new_levels=T)
  pred_mat = cbind(data.frame(samples = c(1:1000)), pred_mat)
  pred_mat = pred_mat %>%
    pivot_longer(!samples, names_to = "var", values_to = "val") %>%
    left_join(data.frame(var = as.character(c(1: nrow(m_df[[1]]))), code = m_df[[1]][[variable]]))
  sum_pred = pred_mat %>%
    group_by(code) %>%
    dplyr::summarise(
      low95 = quantile(val, prob = 0.025),
      low80 = quantile(val, prob = 0.1),
      low50 = quantile(val, prob = 0.25),
      med = quantile(val, prob = 0.5),
      high50 = quantile(val, prob = 0.75),
      high80 = quantile(val, prob = 0.9),
      high95 = quantile(val, prob = 0.975)
    )
  return(sum_pred)
}

diff_sum = function(model_gpt, model_cnn, variable, samples = 1000){
  gpt_df = conditional_effects(model_gpt, variable, ndraws = 1000, method = "posterior_epred")
  pred_mat_gpt = posterior_epred(model_gpt, newdata = gpt_df[[1]], ndraws = 1000, allow_new_levels=T)
  
  cnn_df = conditional_effects(model_cnn, variable, ndraws = 1000, method = "posterior_epred")
  pred_mat_cnn = posterior_epred(model_cnn, newdata = cnn_df[[1]], ndraws = 1000,allow_new_levels=T)
  
  pred_diff = pred_mat_gpt - pred_mat_cnn
  pred_diff = cbind(data.frame(samples = c(1:1000)), pred_diff)
  pred_diff = pred_diff %>%
    pivot_longer(!samples, names_to = "var", values_to = "val") %>%
    left_join(data.frame(var = as.character(c(1: ncol(pred_mat_gpt))), code = gpt_df[[1]][[variable]]))
  sum_pred = pred_diff %>%
    group_by(code) %>%
    dplyr::summarise(
      low95 = quantile(val, prob = 0.025),
      low80 = quantile(val, prob = 0.1),
      low50 = quantile(val, prob = 0.25),
      med = quantile(val, prob = 0.5),
      high50 = quantile(val, prob = 0.75),
      high80 = quantile(val, prob = 0.9),
      high95 = quantile(val, prob = 0.975)
    )
  return(sum_pred)
}

diff_draws = function(model_gpt, model_cnn, variable, samples = 1000){
  gpt_df = conditional_effects(model_gpt, variable, ndraws = 1000, method = "posterior_epred")
  pred_mat_gpt = posterior_epred(model_gpt, newdata = gpt_df[[1]], ndraws = 1000, allow_new_levels=T)
  
  cnn_df = conditional_effects(model_cnn, variable, ndraws = 1000, method = "posterior_epred")
  pred_mat_cnn = posterior_epred(model_cnn, newdata = cnn_df[[1]], ndraws = 1000, allow_new_levels=T)
  
  pred_diff = pred_mat_gpt - pred_mat_cnn
  pred_diff = cbind(data.frame(samples = c(1:1000)), pred_diff)
  pred_diff = pred_diff %>%
    pivot_longer(!samples, names_to = "var", values_to = "val") %>%
    left_join(data.frame(var = as.character(c(1: ncol(pred_mat_gpt))), code = gpt_df[[1]][[variable]]))
  sum_pred = pred_diff %>%
    group_by(code) %>%
    # dplyr::summarise(cnn_win = sum(val < -0.5), gpt_win = sum(val > 0.5))
    dplyr::summarise(cnn_win = sum(val < -0.2), gpt_win = sum(val > 0.2))
  return(sum_pred)
}
get_credible_intervals <- function() {
  # Returns posterior means and quantiles for each observation
  fitted_gpt <- fitted(m_gpt, summary = TRUE, probs = c(0.025, 0.975))
  fitted_cnn <- fitted(m_cnn, summary = TRUE, probs = c(0.025, 0.975))
  
  # Compute half-width of credible interval
  ci_gpt_halfwidth <- (fitted_gpt[, "Q97.5"] - fitted_gpt[, "Q2.5"]) / 2
  ci_cnn_halfwidth <- (fitted_cnn[, "Q97.5"] - fitted_cnn[, "Q2.5"]) / 2
  
  median_ci_gpt <- median(ci_gpt_halfwidth)
  median_ci_cnn <- median(ci_cnn_halfwidth)
  
  # Summarize
  cat("GPT mean ±:", round(mean(ci_gpt_halfwidth), 3), "\n")
  cat("CNN mean ±:", round(mean(ci_cnn_halfwidth), 3), "\n")
  cat("GPT Median ±:", round(median_ci_gpt, 3), "\n")
  cat("CNN Median ±:", round(median_ci_cnn, 3), "\n")
  
}
get_family <- function(info) {
  lineage <- info$lineage
  family <- lineage[sapply(lineage, function(x) x$rank == "family")]
  if (length(family) == 1) return(family[[1]]$name)
  return(NA)
}
get_order <- function(info) {
  lineage <- info$lineage
  order <- lineage[sapply(lineage, function(x) x$rank == "order")]
  if (length(order) == 1) return(order[[1]]$name)
  return(NA)
}
get_class <- function(info) {
  lineage <- info$lineage
  class <- lineage[sapply(lineage, function(x) x$rank == "class")]
  if (length(class) == 1) return(class[[1]]$name)
  return(NA)
}

##### Data processing #####S
df <- read.csv("../data/final_image_level_data_v6.csv")
df$invasive_status[df$invasive_status == "yes"] <- "invasive"
df$invasive_status[df$invasive_status == "unknown"] <- "Not in GISD/GRIS"
num_spp <- unique(df$sci_name)
print(num_spp)
#Add a phylogeny
tx_search = tnrs_match_names(names = unique(df$sci_name), context_name = "All life")
ott_in_tree = ott_id(tx_search)[is_in_tree(ott_id(tx_search))]
tr = tol_induced_subtree(ott_ids = ott_in_tree)
tr = ape::compute.brlen(tr)
vcov = vcv.phylo(tr)
saveRDS(vcov, '../data/vcov_observed_v4.RDS')
spec_list = sub(".*ott", "", tr$tip.label)
tx_search = tx_search[tx_search$ott_id %in% spec_list, ] #Only keep data where a phylogeny is available. Remove 2 species
tmp_df = left_join(df, unique(tx_search[,c(2,5)]), by = c("sci_name" = "unique_name")) #Only keeping exact matches, not approximates
tmp_df = tmp_df[which(!is.na(tmp_df$ott_id)),]
tmp_df$tips_chr = gsub(" ", "_", tmp_df$sci_name)
tmp_df$tips_chr = paste0(tmp_df$tips_chr, "_ott", tmp_df$ott_id)
phylo_df = tmp_df

#transform any required variables
phylo_df$img_size_px = phylo_df$img_size_px/100000
phylo_df$bbox_size_px_log = log(phylo_df$bbox_size_px)
phylo_df$bbox_proportion_log = log(phylo_df$bbox_proportion)
phylo_df$bbox_touches_edge_fac = as.factor(phylo_df$bbox_touches_edge)
phylo_df$isGreyScale_fac = as.factor(phylo_df$isGreyScale)
phylo_df$body_log = log(phylo_df$body_mass_kg)
phylo_df$range_log = log(phylo_df$range_area_km)
phylo_df$correct_or_not_gpt = as.numeric(phylo_df$correct_or_not_gpt)
phylo_df$correct_cnn = as.numeric(phylo_df$correct_cnn)

#Look for correlation in the continuous space
colnames(phylo_df)
cor(phylo_df[,c("img_size_px", "bbox_size_px_log", "bbox_proportion_log", "body_log", "range_log")])

drops <- c("isNight", "bbox_size_px_log")
df <- df[ , !(names(df) %in% drops)]
unique(phylo_df$sci_name)

phylo_df$iucn_category[phylo_df$iucn_category == ""] <- "NOT_EVALUATED"
phylo_df$invasive_status <- as.factor(phylo_df$invasive_status)
phylo_df$invasive_status <- relevel(phylo_df$invasive_status, ref = "native")

#Develop model structures
form_gpt = bf(correct_or_not_gpt ~ scale(body_log) + scale(range_log) + iucn_category + (1|sci_name) + (1|location) + (1|gr(tips_chr, cov = A)))
m_gpt = brm(form_gpt, data = phylo_df, data2 = list(A = vcov), family = bernoulli(link = "logit"), iter = 5000, chains = 3, cores = 3, control = list(adapt_delta = 0.99))
summary(m_gpt)
form_cnn = bf(correct_cnn ~ scale(body_log) + scale(range_log) + iucn_category + (1|sci_name) + (1|location) + (1|gr(tips_chr, cov = A)))
m_cnn = brm(form_cnn, data = phylo_df, data2 = list(A = vcov), family = bernoulli(link = "logit"), iter = 5000, chains = 3, cores = 3, control = list(adapt_delta = 0.99))
summary(m_cnn)

# model proof - 80% train 20% test
set.seed(42)
phylo_df$id <- 1:nrow(phylo_df)
phylo_df_train <- phylo_df %>% sample_frac(0.8)
phylo_df_test <- anti_join(phylo_df, phylo_df_train, by='id')
m_gpt_partial = brm(form_gpt, data = phylo_df_train, data2 = list(A = vcov), family = bernoulli(link = "logit"), iter = 5000, chains = 3, cores = 3, control = list(adapt_delta = 0.99))
summary(m_gpt_partial)
m_cnn_partial = brm(form_cnn, data = phylo_df_train, data2 = list(A = vcov), family = bernoulli(link = "logit"), iter = 5000, chains = 3, cores = 3, control = list(adapt_delta = 0.99))
summary(m_cnn_partial)

# Model theoretical average
intercept_gpt <- fixef(m_gpt_partial)["Intercept", "Estimate"]
intercept_cnn <- fixef(m_cnn_partial)["Intercept", "Estimate"]
probability_gpt <- 1 / (1 + exp(-intercept_gpt))
probability_cnn <- 1 / (1 + exp(-intercept_cnn))
# Convert to percentage
percentage_gpt_train_intercept <- probability_gpt * 100
percentage_cnn_train_intercept <- probability_cnn * 100
cat("The percentage accuracy score for 'average' input data is", percentage_gpt_train_intercept, "% for GPT and ", percentage_cnn_train_intercept, "% for CNN.\n")
# Averaged over all observations in the set
fitted_gpt_train <- fitted(m_gpt_partial, summary = TRUE)[, "Estimate"]
fitted_cnn_train <- fitted(m_cnn_partial, summary = TRUE)[, "Estimate"]
# Average over all observations to get expected accuracy
mean_accuracy_gpt_train <- mean(fitted_gpt_train)
mean_accuracy_cnn_train <- mean(fitted_cnn_train)
# Convert to percentages
percentage_gpt_train <- mean_accuracy_gpt_train * 100
percentage_cnn_train <- mean_accuracy_cnn_train * 100
# Print results
cat("Average predicted accuracy across the dataset is:\n")
cat("GPT: ", round(percentage_gpt_train, 1), "%\n")
cat("CNN: ", round(percentage_cnn_train, 1), "%\n")
# want to actually predict over the test set also
pred_samples_gpt <- predict(m_gpt_partial, newdata = phylo_df_test, allow_new_levels = TRUE, summary = FALSE)
pred_samples_cnn <- predict(m_cnn_partial, newdata = phylo_df_test, allow_new_levels = TRUE, summary = FALSE)
# Back-transform from logit to probability
intercept_gpt <- fixef(m_gpt)["Intercept", "Estimate"]
intercept_cnn <- fixef(m_cnn)["Intercept", "Estimate"]
probability_gpt <- 1 / (1 + exp(-intercept_gpt))
probability_cnn <- 1 / (1 + exp(-intercept_cnn))
percentage_gpt <- probability_gpt * 100
percentage_cnn <- probability_cnn * 100
# Print the result
# cat("The percentage accuracy score for the 'average' input data is:", percentage, "%\n")
cat("The percentage accuracy score for 'average' input data is", percentage_gpt, "% for GPT and ", percentage_cnn, "% for CNN.\n")

# Get fitted probabilities for each observation
fitted_gpt <- fitted(m_gpt, summary = TRUE)[, "Estimate"]
fitted_cnn <- fitted(m_cnn, summary = TRUE)[, "Estimate"]

# Average over all observations to get expected accuracy
mean_accuracy_gpt <- mean(fitted_gpt)
mean_accuracy_cnn <- mean(fitted_cnn)

# Convert to percentages
percentage_gpt <- mean_accuracy_gpt * 100
percentage_cnn <- mean_accuracy_cnn * 100

# Print results
cat("Average predicted accuracy across the dataset is:\n")
cat("GPT: ", round(percentage_gpt, 1), "%\n")
cat("CNN: ", round(percentage_cnn, 1), "%\n")

# Extract and view fixed effects with custom quantiles
fixef(m_gpt, summary = TRUE, probs = c(0.025, 0.5, 0.975))
get_credible_intervals()

# need to get the percentage of data correct or not here
percent_image_coverage_gpt <- df %>% filter(correct_or_not_gpt == TRUE)
percent_image_coverage_gpt <- nrow(percent_image_coverage_gpt) / nrow(df)
percent_image_coverage_gpt

percent_image_coverage_cnn <- df %>% filter(correct_cnn == TRUE)
percent_image_coverage_cnn <- nrow(percent_image_coverage_cnn) / nrow(df)
percent_image_coverage_cnn

## with 80/20 split we get across data predicted accuracy of GPT 53.1% CNN 77.2%, 100% data use GPT 52.9% CNN 77.3%, actual coverage is GPT 53.0% CNN 72.2% ##

# read in the imputed models
imputed_m_gpt <- readRDS("../isca/m_gpt_isca_v9_native_ref.RDS")
imputed_m_cnn <- readRDS("../isca/m_cnn_isca_v9_native_ref.RDS")
# give the above, we can be reasonably confident of our imputed models
intercept_gpt_imputed <- fixef(imputed_m_gpt)["Intercept", "Estimate"]
intercept_cnn_imputed <- fixef(imputed_m_cnn)["Intercept", "Estimate"]
probability_gpt_imputed <- 1 / (1 + exp(-intercept_gpt_imputed))
probability_cnn_imputed <- 1 / (1 + exp(-intercept_cnn_imputed))

# Convert to percentage
percentage_gpt_imputed <- probability_gpt_imputed * 100
percentage_cnn_imputed <- probability_cnn_imputed * 100

# Print the result
# cat("The percentage accuracy score for the 'average' input data is:", percentage, "%\n")
cat("The percentage accuracy score for 'average' input data is", percentage_gpt_imputed, "% for GPT and ", percentage_cnn_imputed, "% for CNN.\n")

# Get fitted probabilities for each observation
fitted_gpt_imputed <- fitted(imputed_m_gpt, summary = TRUE)[, "Estimate"]
fitted_cnn_imputed <- fitted(imputed_m_cnn, summary = TRUE)[, "Estimate"]

# Average over all observations to get expected accuracy
mean_accuracy_gpt_imputed <- mean(fitted_gpt_imputed)
mean_accuracy_cnn_imputed <- mean(fitted_cnn_imputed)

# Convert to percentages
percentage_gpt_imputed <- mean_accuracy_gpt_imputed * 100
percentage_cnn_imputed <- mean_accuracy_cnn_imputed * 100

# Print results
cat("Average predicted accuracy across the dataset is:\n")
cat("GPT: ", round(percentage_gpt_imputed, 1), "%\n")
cat("CNN: ", round(percentage_cnn_imputed, 1), "%\n")

#### Plots #####
### Prep ###
# Body Mass
body_mass_cnn = marg_sum(m_cnn, "body_log", samples = 1000)
body_mass_gpt = marg_sum(m_gpt, "body_log", samples = 1000)
imputed_body_mass_cnn = marg_sum(imputed_m_cnn, "body_log", samples = 1000)
imputed_body_mass_gpt = marg_sum(imputed_m_gpt, "body_log", samples = 1000)

# Range Size
range_size_cnn = marg_sum(m_cnn, "range_log", samples = 1000)
range_size_gpt = marg_sum(m_gpt, "range_log", samples = 1000)
imputed_range_size_cnn = marg_sum(imputed_m_cnn, "range_log", samples = 1000)
imputed_range_size_gpt = marg_sum(imputed_m_gpt, "range_log", samples = 1000)

# IUCN
iucn_cnn = marg_sum(m_cnn, "iucn_category", samples = 1000)
iucn_cnn$code = gsub("_", " ", iucn_cnn$code)
iucn_cnn$code = factor(iucn_cnn$code, levels = c("CRITICALLY ENDANGERED", "ENDANGERED", "VULNERABLE", "NEAR THREATENED", "LEAST CONCERN", "DATA DEFICIENT", "NOT EVALUATED"))
iucn_gpt = marg_sum(m_gpt, "iucn_category", samples = 1000)
iucn_gpt$code = gsub("_", " ", iucn_gpt$code)
iucn_gpt$code = factor(iucn_gpt$code, levels = c("CRITICALLY ENDANGERED", "ENDANGERED", "VULNERABLE", "NEAR THREATENED", "LEAST CONCERN", "DATA DEFICIENT", "NOT EVALUATED"))
# imputed
imputed_iucn_cnn = marg_sum(imputed_m_cnn, "iucn_category", samples = 1000)
imputed_iucn_cnn$code = gsub("_", " ", imputed_iucn_cnn$code)
imputed_iucn_cnn$code = factor(imputed_iucn_cnn$code, levels = c("CRITICALLY ENDANGERED", "ENDANGERED", "VULNERABLE", "NEAR THREATENED", "LEAST CONCERN", "DATA DEFICIENT", "NOT EVALUATED"))
imputed_iucn_gpt = marg_sum(imputed_m_gpt, "iucn_category", samples = 1000)
imputed_iucn_gpt$code = gsub("_", " ", imputed_iucn_gpt$code)
imputed_iucn_gpt$code = factor(imputed_iucn_gpt$code, levels = c("CRITICALLY ENDANGERED", "ENDANGERED", "VULNERABLE", "NEAR THREATENED", "LEAST CONCERN", "DATA DEFICIENT", "NOT EVALUATED"))

# get the data matched up
body_mass_cnn$source <- "Observed"
imputed_body_mass_cnn$source <- "Imputed"
body_mass_combined_cnn <- bind_rows(body_mass_cnn, imputed_body_mass_cnn)
body_mass_combined_cnn$model <- "CNN"
body_mass_combined_cnn$variable <- "Body Mass"

range_size_cnn$source <- "Observed"
imputed_range_size_cnn$source <- "Imputed"
range_size_combined_cnn <- bind_rows(range_size_cnn, imputed_range_size_cnn)
range_size_combined_cnn$model <- "CNN"
range_size_combined_cnn$variable <- "Range Size"

iucn_cnn$source <- "observed"
imputed_iucn_cnn$source <- "imputed"
iucn_combined_data_cnn <- bind_rows(
  iucn_cnn %>% filter(!code %in% c("NOT EVALUATED", "DATA DEFICIENT")),
  imputed_iucn_cnn %>% filter(!code %in% c("NOT EVALUATED", "DATA DEFICIENT"))
)
iucn_combined_data_cnn$model <- "CNN"
iucn_combined_data_cnn$variable <- "IUCN Status"

body_mass_gpt$source <- "Observed"
imputed_body_mass_gpt$source <- "Imputed"
body_mass_combined_gpt <- bind_rows(body_mass_gpt, imputed_body_mass_gpt)
body_mass_combined_gpt$model <- "GPT"
body_mass_combined_gpt$variable <- "Body Mass"

range_size_gpt$source <- "Observed"
imputed_range_size_gpt$source <- "Imputed"
range_size_combined_gpt <- bind_rows(range_size_gpt, imputed_range_size_gpt)
range_size_combined_gpt$model <- "GPT"
range_size_combined_gpt$variable <- "Range Size"

iucn_gpt$source <- "observed"
imputed_iucn_gpt$source <- "imputed"
iucn_combined_data_gpt <- bind_rows(
  iucn_gpt %>% filter(!code %in% c("NOT EVALUATED", "DATA DEFICIENT")),
  imputed_iucn_gpt %>% filter(!code %in% c("NOT EVALUATED", "DATA DEFICIENT"))
)
iucn_combined_data_gpt$model <- "GPT"
iucn_combined_data_gpt$variable <- "IUCN Status"

# merge the dfs 
body_mass_all <- rbind(body_mass_combined_cnn, body_mass_combined_gpt)
range_size_all <- rbind(range_size_combined_cnn, range_size_combined_gpt)
iucn_all <- rbind(iucn_combined_data_cnn, iucn_combined_data_gpt)
iucn_all$source[iucn_all$source == "observed"] <- "Observed"
iucn_all$source[iucn_all$source == "imputed"] <- "Imputed"
body_mass_all$source[body_mass_all$source == "imputed"] <- "Imputed"
range_size_all$source[range_size_all$source == "imputed"] <- "Imputed"
body_mass_all$source[body_mass_all$source == "observed"] <- "Observed"
range_size_all$source[range_size_all$source == "observed"] <- "Observed"
# all_var_df <- rbind(body_mass_all, range_size_all)
# all_var_df <- rbind(all_var_df, iucn_all)
# all_var_df$source[all_var_df$source == "observed"] <- "Observed"
# all_var_df$source[all_var_df$source == "imputed"] <- "Imputed"
range_size_all$facet_label <- factor(
  interaction(range_size_all$variable, range_size_all$model),
  labels = c("B", "E")  # For range_plot
)

iucn_all$facet_label <- factor(
  iucn_all$model,
  labels = c("C", "F")  # For iucn_plot
)

body_mass_all$facet_label <- factor(
  interaction(body_mass_all$variable, body_mass_all$model),
  labels = c("CNN:    A", "GPT:   D")  # For body_plot
)

common_colour_scale <- scale_colour_manual(
  values = c("Observed" = "#f2c00a", "Imputed" = "#40b8a0"),
  name = "Source",
  labels = c("Observed", "Imputed")
)

body_plot <- ggplot(body_mass_all, aes(x = code, y = med, colour = source)) +
  geom_line(linewidth = 1.2) +
  # facet_wrap(vars(model), scales="fixed", ncol = 1) +
  facet_wrap(~ facet_label, ncol = 1, labeller = label_value) +
  # scale_colour_manual(values = c("Observed" = "#f2c00a", "Imputed" = "#40b8a0")) +
  common_colour_scale +
  guides(fill = "none", colour = guide_legend()) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  # labs(y = "Probability of correct classification", x = "Log Body Mass (kg)", title = "Body Mass: Observed vs Imputed CNN", colour = "Source") +
  labs(y = "Probability of correct classification", x = "Log Body Mass (kg)") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(linewidth = 1, fill = NA),
        panel.grid = element_blank(),
        axis.text = element_text(colour = "black", size = 20),
        axis.title = element_text(size = 20),
        # legend.title = element_text(size = 15),
        # legend.text = element_text( size = 15),
        legend.position = "none",
        strip.text.x = element_text(
          size = 20, color = "black", face = "bold"),
        aspect.ratio = 1)

body_plot

range_plot <- ggplot(range_size_all, aes(x = code, y = med, colour = source)) +
  geom_line(linewidth = 1.2) +
  # facet_wrap(vars(model), scales="fixed", ncol = 1) +
  facet_wrap(~ facet_label, ncol = 1, labeller = label_value) +
  # scale_colour_manual(values = c("Observed" = "#f2c00a", "Imputed" = "#40b8a0")) +
  common_colour_scale +
  guides(fill = "none", colour = guide_legend()) +
  guides(fill = "none", colour = guide_legend()) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  # labs(y = "Probability of correct classification", x = "Log Body Mass (kg)", title = "Body Mass: Observed vs Imputed CNN", colour = "Source") +
  labs(x = "Log Range Size (km²)") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(linewidth = 1, fill = NA),
        panel.grid = element_blank(),
        axis.text = element_text(colour = "black", size = 20),
        axis.title = element_text(size = 20),
        # legend.title = element_text(size = 15),
        # legend.text = element_text( size = 15),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.x = element_text(
          size = 20, color = "black", face = "bold"),
        aspect.ratio = 1)

range_plot

iucn_plot <- ggplot(iucn_all, aes(x = code, y = med, group = source, colour = source)) +
  # facet_wrap(vars(model), scales="fixed", ncol=1) +
  facet_wrap(~ facet_label, ncol = 1, labeller = label_value) +
  geom_linerange(aes(ymin = low95, ymax = high95), 
                 alpha = 0.2, linewidth = 1, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = low80, ymax = high80), 
                 alpha = 0.3, linewidth = 2, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = low50, ymax = high50), 
                 alpha = 0.5, linewidth = 3, position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), size = 3, colour="black") +
  common_colour_scale +
  scale_x_discrete(labels = c("CR", "EN", "VU", "NT", "LC")) +
  # labs(y = "Probability of correct classification", x = "IUCN Status", 
  #      title = "IUCN Status: Observed vs Imputed GPT", colour = "Source") +
  labs(x = "IUCN Status") + 
  theme(panel.background = element_blank(),
                          panel.border = element_rect(linewidth = 1, fill = NA),
                          panel.grid = element_blank(),
                          axis.text = element_text(colour = "black", size = 20),
                          axis.title = element_text(size = 20),
                          axis.title.y = element_blank(),
                          axis.text.y = element_blank(),
                          axis.ticks.y = element_blank(),
                          legend.position = "none",
                          strip.text.x = element_text(
                            size = 20, color = "black", face = "bold"),
                          # legend.title = element_text(size = 15),
                          # legend.text = element_text( size = 15),
                          aspect.ratio = 1)
iucn_plot
iucn_all[iucn_all$code == "CRITICALLY ENDANGERED",]
iucn_all[iucn_all$code == "ENDANGERED",]
# covariates_plot <- body_plot + range_plot + iucn_plot # need to add a common legend for the plot
# also iucn plots are a different size??
covariates_plot <- (body_plot + range_plot + iucn_plot)
# covariates_plot <- (body_plot + range_plot + iucn_plot) +
#   plot_layout(guides = "collect") &
#   theme(legend.position = "right")
covariates_plot

common_fill_scale <- scale_fill_manual(
  values = c("Observed" = "#f2c00a", "Imputed" = "#40b8a0"),
  name = "Source",
  labels = c("Observed", "Imputed")
)
legend_df <- data.frame(ID = c(1, 2, 3, 4, 5),
                        Source = c('Observed', 'Observed', 'Imputed', 'Imputed', 'Imputed'),
                  y_vals = c(1, 2, 3, 4, 5))
legend_df$Source <- factor(legend_df$Source, levels = c("Observed", "Imputed"))

legend_mulit_plot <- ggplot(legend_df, aes(x = ID, y = y_vals, fill=Source)) +
  geom_bar(stat='identity') +
  common_fill_scale +
  theme(legend.text = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.key.size = unit(0.75, 'cm'))
legend_mulit_plot
leg <- get_legend(legend_mulit_plot)
multi_plot_legend <- as_ggplot(leg)

# multi_plot_legend
covariates_plot <- plot_grid(covariates_plot, multi_plot_legend, ncol = 2, rel_widths = c(3, 0.5))
covariates_plot
ggsave(covariates_plot, filename="../results/final/results/multiplot_traits.pdf", device="pdf", units="in", width=10, height=8, limitsize = FALSE)

# covariates_plot <- ggarrange(body_plot, range_plot, iucn_plot, labels=c("A", "B", "C", "D", "E", "F"), common.legend = TRUE, legend = "right", ncol = 3, nrow = 2)

A <- ggplot(body_mass_combined_cnn, aes(x = code, y = med, colour = source)) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = c("Observed" = "#f2c00a", "Imputed" = "#40b8a0")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  # labs(y = "Probability of correct classification", x = "Log Body Mass (kg)", title = "Body Mass: Observed vs Imputed CNN", colour = "Source") +
  labs(y = "Probability of correct classification", x = "Log Body Mass (kg)", colour = "Source") +
  theme_classic() + theme(panel.background = element_blank(),
                         panel.border = element_rect(linewidth = 1, fill = NA),
                         panel.grid = element_blank(),
                         axis.text = element_text(colour = "black", size = 15),
                         axis.title = element_text(size = 15),
                         # legend.position = "none",
                         legend.title = element_text(size = 15),
                         legend.text = element_text( size = 15),
                         aspect.ratio = 1)
B <- ggplot(range_size_combined_cnn, aes(x = code, y = med, colour = source)) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = c("Observed" = "#f2c00a", "Imputed" = "#40b8a0")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  # labs(y = "Probability of correct classification", x = "Log Range Size (km²)", title = "Range Size: Observed vs Imputed CNN", colour = "Source") +
  labs(y = "Probability of correct classification", x = "Log Range Size (km²)", colour = "Source") +
  theme_classic() + theme(panel.background = element_blank(),
                          panel.border = element_rect(linewidth = 1, fill = NA),
                          panel.grid = element_blank(),
                          axis.text = element_text(colour = "black", size = 15),
                          axis.title = element_text(size = 15),
                          legend.position = "none",
                          axis.title.y = element_blank(),
                          axis.text.y = element_blank(),
                          axis.ticks.y = element_blank(),
                          aspect.ratio = 1)
C <- ggplot(data = iucn_combined_data_cnn, aes(x = code, y = med, group = source, colour = source)) +
  geom_linerange(aes(ymin = low95, ymax = high95), 
                 alpha = 0.2, linewidth = 1, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = low80, ymax = high80), 
                 alpha = 0.3, linewidth = 2, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = low50, ymax = high50), 
                 alpha = 0.5, linewidth = 3, position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), size = 3, colour="black") +
  scale_colour_manual(labels = c("Imputed","Observed"), values = c("observed" = "#f2c00a", "imputed" = "#40b8a0")) +
  scale_x_discrete(labels = c("CR", "EN", "VU", "NT", "LC")) +
  # labs(y = "Probability of correct classification", x = "IUCN Status", 
  #     title = "IUCN Status: Observed vs Imputed CNN", colour = "Source") +
  labs(y = "Probability of correct classification", x = "IUCN Status", colour = "Source") +
  theme_classic() + theme(panel.background = element_blank(),
                            panel.border = element_rect(linewidth = 1, fill = NA),
                            panel.grid = element_blank(),
                            axis.text = element_text(colour = "black", size = 15),
                            axis.title = element_text(size = 15),
                            legend.position = "none",
                            axis.title.y = element_blank(),
                            axis.text.y = element_blank(),
                            axis.ticks.y = element_blank(),
                            aspect.ratio = 1)
D <- ggplot(body_mass_combined_gpt, aes(x = code, y = med, colour = source)) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = c("Observed" = "#f2c00a", "Imputed" = "#40b8a0")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  # labs(y = "Probability of correct classification", x = "Log Body Mass (kg)", title = "Body Mass: Observed vs Imputed GPT", colour = "Source") +
  labs(y = "Probability of correct classification", x = "Log Body Mass (kg)", colour = "Source") +
  theme_classic() + theme(panel.background = element_blank(),
                          panel.border = element_rect(linewidth = 1, fill = NA),
                          panel.grid = element_blank(),
                          axis.text = element_text(colour = "black", size = 15),
                          axis.title = element_text(size = 15),
                          legend.position = "none",
                          aspect.ratio = 1)
E <- ggplot(range_size_combined_gpt, aes(x = code, y = med, colour = source)) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = c("Observed" = "#f2c00a", "Imputed" = "#40b8a0")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  # labs(y = "Probability of correct classification", x = "Log Range Size (km²)", title = "Range Size: Observed vs Imputed GPT", colour = "Source") +
  labs(y = "Probability of correct classification", x = "Log Range Size (km²)", colour = "Source") +
  theme_classic() + theme(panel.background = element_blank(),
                          panel.border = element_rect(linewidth = 1, fill = NA),
                          panel.grid = element_blank(),
                          axis.text = element_text(colour = "black", size = 15),
                          axis.title = element_text(size = 15),
                          legend.position = "none",                          
                          axis.title.y = element_blank(),
                          axis.text.y = element_blank(),
                          axis.ticks.y = element_blank(),
                          aspect.ratio = 1)
G <- ggplot(data = iucn_combined_data_gpt, aes(x = code, y = med, group = source, colour = source)) +
  geom_linerange(aes(ymin = low95, ymax = high95), 
                 alpha = 0.2, linewidth = 1, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = low80, ymax = high80), 
                 alpha = 0.3, linewidth = 2, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = low50, ymax = high50), 
                 alpha = 0.5, linewidth = 3, position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), size = 3, colour="black") +
  scale_colour_manual(labels = c("Imputed","Observed"), values = c("observed" = "#f2c00a", "imputed" = "#40b8a0")) +
  scale_x_discrete(labels = c("CR", "EN", "VU", "NT", "LC")) +
  # labs(y = "Probability of correct classification", x = "IUCN Status", 
  #      title = "IUCN Status: Observed vs Imputed GPT", colour = "Source") +
  labs(x = "IUCN Status", colour = "Source") +
  theme_classic() + theme(panel.background = element_blank(),
                          panel.border = element_rect(linewidth = 1, fill = NA),
                          panel.grid = element_blank(),
                          axis.text = element_text(colour = "black", size = 15),
                          axis.title = element_text(size = 15),
                          legend.position = "none",
                          axis.title.y = element_blank(),
                          axis.text.y = element_blank(),
                          axis.ticks.y = element_blank(),
                          aspect.ratio = 1)
H <- ggarrange(A, B, C, D, E, G, labels=c("A", "B", "C", "D", "E", "F"), common.legend = TRUE, legend = "right", ncol = 3, nrow = 2)
# H <- (A + B + C) / (D + E + G)
# H <- H + theme(legend.position = "top")
H
ggsave(H, filename="../results/final/results/multiplot_traits.pdf", device="pdf", units="in", width=15, height=10, limitsize = FALSE)
# get the diff draws out for IUCN category, as these are still relevant I think
diff_draws(m_gpt, m_cnn, "iucn_category")






# now just do this on the imputed models, get a 2D colour map for each tip (both good in top right corner of the map, both bad in bottom left, etc.)


# scaled_phylo_df = phylo_df
scaled_phylo_df <- read.csv('../data/phylo_df_all_v5.csv')
# ok now remove all marine, non-mammals, bats and moles from this 
# use the IUCN maps to remove the marine
# query rotl to get all the bats and moles? 
tx_search <- tnrs_match_names(names = scaled_phylo_df$sci_name, context_name = "All life")
ott_ids <- tx_search$ott_id
lineages <- taxonomy_taxon_info(ott_ids, include_lineage = TRUE)
families <- sapply(lineages, get_family)
orders <- sapply(lineages, get_order)
# infraorders <- sapply(lineages, get_infraorder)
classes <- sapply(lineages, get_class)
names(families) <- tx_search$unique_name
names(orders) <-tx_search$unique_name
# names(infraorders) <-tx_search$unique_name
names(classes) <-tx_search$unique_name
scaled_phylo_df$family <- families[scaled_phylo_df$sci_name]
scaled_phylo_df$order <- orders[scaled_phylo_df$sci_name]
scaled_phylo_df$class <- classes[scaled_phylo_df$sci_name]
scaled_phylo_df$family[scaled_phylo_df$species == "Amblysomus corriae" & is.na(scaled_phylo_df$family)] <- "Chrysochloridae"
scaled_phylo_df$family[scaled_phylo_df$species == "Amblysomus hottentotus" & is.na(scaled_phylo_df$family)] <- "Chrysochloridae"
scaled_phylo_df$family[scaled_phylo_df$species == "Amblysomus marleyi" & is.na(scaled_phylo_df$family)] <- "Chrysochloridae"
scaled_phylo_df$family[scaled_phylo_df$species == "Amblysomus robustus" & is.na(scaled_phylo_df$family)] <- "Chrysochloridae"
scaled_phylo_df$family[scaled_phylo_df$species == "Amblysomus septentrionalis" & is.na(scaled_phylo_df$family)] <- "Chrysochloridae"
scaled_phylo_df$family[scaled_phylo_df$species == "Calcochloris obtusirostris" & is.na(scaled_phylo_df$family)] <- "Chrysochloridae"
scaled_phylo_df$family[scaled_phylo_df$species == "Calcochloris tytonis" & is.na(scaled_phylo_df$family)] <- "Chrysochloridae"
scaled_phylo_df$family[scaled_phylo_df$species == "Carpitalpa arendsi" & is.na(scaled_phylo_df$family)] <- "Chrysochloridae"
scaled_phylo_df$family[scaled_phylo_df$species == "Chlorotalpa duthieae" & is.na(scaled_phylo_df$family)] <- "Chrysochloridae"
scaled_phylo_df$family[scaled_phylo_df$species == "Chlorotalpa sclateri" & is.na(scaled_phylo_df$family)] <- "Chrysochloridae"
scaled_phylo_df$family[scaled_phylo_df$species == "Chrysochloris asiatica" & is.na(scaled_phylo_df$family)] <- "Chrysochloridae"
scaled_phylo_df$family[scaled_phylo_df$species == "Chrysochloris stuhlmanni" & is.na(scaled_phylo_df$family)] <- "Chrysochloridae"
scaled_phylo_df$family[scaled_phylo_df$species == "Chrysochloris visagiei" & is.na(scaled_phylo_df$family)] <- "Chrysochloridae"
scaled_phylo_df$family[scaled_phylo_df$species == "Chrysospalax trevelyani" & is.na(scaled_phylo_df$family)] <- "Chrysochloridae"
scaled_phylo_df$family[scaled_phylo_df$species == "Chrysospalax villosus" & is.na(scaled_phylo_df$family)] <- "Chrysochloridae"
scaled_phylo_df$family[scaled_phylo_df$species == "Cryptochloris wintoni" & is.na(scaled_phylo_df$family)] <- "Chrysochloridae"
scaled_phylo_df$family[scaled_phylo_df$species == "Cryptochloris zyli" & is.na(scaled_phylo_df$family)] <- "Chrysochloridae"
scaled_phylo_df$family[scaled_phylo_df$species == "Echinops telfairi" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Eremitalpa granti" & is.na(scaled_phylo_df$family)] <- "Chrysochloridae"
scaled_phylo_df$family[scaled_phylo_df$species == "Geogale aurita" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Hemicentetes nigriceps" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Hemicentetes semispinosus" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Limnogale mergulus" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Microgale brevicaudata" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Microgale cowani" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Microgale dobsoni" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Microgale drouhardi" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Microgale dryas" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Microgale fotsifotsy" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Microgale gracilis" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Microgale grandidieri" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Microgale gymnorhyncha" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Microgale jenkinsae" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Microgale jobihely" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Microgale longicaudata" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Microgale majori" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Microgale monticola" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Microgale nasoloi" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Microgale parvula" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Microgale principula" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Microgale pusilla" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Microgale soricoides" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Microgale taiva" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Microgale talazaci" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Microgale thomasi" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Micropotamogale lamottei" & is.na(scaled_phylo_df$family)] <- "Potamogalidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Micropotamogale ruwenzorii" & is.na(scaled_phylo_df$family)] <- "Potamogalidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Neamblysomus gunningi" & is.na(scaled_phylo_df$family)] <- "Chrysochloridae"
scaled_phylo_df$family[scaled_phylo_df$species == "Neamblysomus julianae" & is.na(scaled_phylo_df$family)] <- "Chrysochloridae"
scaled_phylo_df$family[scaled_phylo_df$species == "Oryzorictes hova" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Oryzorictes tetradactylus" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Potamogale velox" & is.na(scaled_phylo_df$family)] <- "Potamogalidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Setifer setosus" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"
scaled_phylo_df$family[scaled_phylo_df$species == "Tenrec ecaudatus" & is.na(scaled_phylo_df$family)] <- "Tenrecidae"

# non-mammals 
scaled_phylo_df <- scaled_phylo_df %>% filter(class == "Mammalia")

# marine mammals
scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Phocidae")
scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Odobenidae")
scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Otariidae")
scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Dugongidae")
scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Trichechidae")
scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Delphinidae")
scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Balaenidae")
scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Neobalaenidae")
scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Balaenopteridae")
scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Cetotheriidae")
scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Iniidae")
scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Kogiidae")
scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Lipotidae")
scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Monodontidae")
scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Phocoenidae")
scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Physeteridae")
scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Platanistidae")
scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Pontoporiidae")
scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Ziphiidae")
scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Eschrichtiidae")
# possibly want to remove sea otters? 
# scaled_phylo_df <- scaled_phylo_df %>% filter(sci_name != "Enhydra lutris")

# moles
# scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Talpidae")
# scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Notoryctidae")
# scaled_phylo_df <- scaled_phylo_df %>% filter(family != "Chrysochloridae")

# bats
scaled_phylo_df <- scaled_phylo_df %>% filter(order != "Chiroptera")



base_df = conditional_effects(imputed_m_gpt, effects = c("range_log"), ndraws = 1000, method = "posterior_epred")[[1]]
base_df = as.data.frame(base_df[1 ,-which(names(base_df) %in% c("range_log", "body_log", "correct_or_not_gpt", "iucn_category", "cond__", "effect1__", "estimate__", "se__", "lower__", "upper__", "sci_name", "tips_chr"))])
# spec_df = unique(scaled_phylo_df[,c("sci_name", "tips_chr", "body_log", "range_log", "invasive_status", "iucn_category")])
spec_df = unique(scaled_phylo_df[,c("sci_name", "tips_chr", "body_log", "range_log", "iucn_category", "family", "order", "class")])
base_df = base_df[rep(seq_len(nrow(base_df)), each = nrow(spec_df)), ]
spec_df = cbind(spec_df, base_df)

spec_df = distinct(spec_df, sci_name, .keep_all = TRUE)

spec_cnn = posterior_epred(imputed_m_cnn, newdata = spec_df, ndraws = 1000, re_formula = ~ (1|sci_name) + (1|gr(tips_chr, cov = A)), allow_new_levels = TRUE)
spec_gpt = posterior_epred(imputed_m_gpt, newdata = spec_df, ndraws = 1000, re_formula =  ~ (1|sci_name) + (1|gr(tips_chr, cov = A)), allow_new_levels = TRUE)
spec_diff = spec_gpt - spec_cnn

spec_cnn = cbind(data.frame(samples = c(1:1000)), spec_cnn)
spec_cnn = spec_cnn %>%
  pivot_longer(!samples, names_to = "var", values_to = "val") %>%
  left_join(data.frame(var = as.character(c(1: nrow(spec_df))), code = spec_df[["sci_name"]]))
sum_spec_cnn = spec_cnn %>%
  group_by(code) %>%
  dplyr::summarise(cnn_prob = mean(val))

spec_gpt = cbind(data.frame(samples = c(1:1000)), spec_gpt)
spec_gpt = spec_gpt %>%
  pivot_longer(!samples, names_to = "var", values_to = "val") %>%
  left_join(data.frame(var = as.character(c(1: nrow(spec_df))), code = spec_df[["sci_name"]]))
sum_spec_gpt = spec_gpt %>%
  group_by(code) %>%
  dplyr::summarise(gpt_prob = mean(val))

spec_diff = cbind(data.frame(samples = c(1:1000)), spec_diff)
spec_diff = spec_diff %>%
  pivot_longer(!samples, names_to = "var", values_to = "val") %>%
  left_join(data.frame(var = as.character(c(1: nrow(spec_df))), code = spec_df[["sci_name"]]))
sum_spec_diff = spec_diff %>%
  group_by(code) %>%
  dplyr::summarise(prob_diff = mean(val))

sum_spec = left_join(sum_spec_cnn, sum_spec_gpt)
sum_spec = left_join(sum_spec, sum_spec_diff)
sum_spec = left_join(sum_spec, spec_df[,c(1,2)], by = c("code" = "sci_name"))

tx_search = tnrs_match_names(names = unique(scaled_phylo_df$sci_name), context_name = "All life")
ott_in_tree = ott_id(tx_search)[is_in_tree(ott_id(tx_search))]

# Generate phylogenetic tree and filter species
tr = tol_induced_subtree(ott_ids = ott_in_tree)
tr = ape::compute.brlen(tr)  # Ensure branch lengths are computed

tr2 = keep.tip(tr, unique(sum_spec$tips_chr))

p = ggtree(tr2, layout = "fan", open.angle = 20) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
p
circles_prob = data.frame(
  CNN = sum_spec$cnn_prob,
  GPT = sum_spec$gpt_prob
)
circles_diff = data.frame(
  DIFF  = sum_spec$prob_diff
)

# Keep only the first occurrence of each name
unique_indices <- !duplicated(sum_spec$tips_chr)
# Subset and preserve data frame structure
circles_prob_dedup <- circles_prob[unique_indices, , drop = FALSE]
rownames(circles_prob_dedup) <- sum_spec$tips_chr[unique_indices]

circles_diff_dedup <- circles_diff[unique_indices, , drop = FALSE]
rownames(circles_diff_dedup) <- sum_spec$tips_chr[unique_indices]

# just make sure they are normalised
norm <- function(x) { (x - min(x)) / (max(x) - min(x))}
a <- norm(circles_prob_dedup$CNN)
b <- norm(circles_prob_dedup$GPT)

# add family to that here
# get families for each row, then order df by grouping?
raw_names <- rownames(circles_prob_dedup)
# Remove _ott###### and replace _ with space
clean_names <- gsub("_ott[0-9]+$", "", raw_names)     # remove suffix
clean_names <- gsub("_", " ", clean_names)  
tx_search <- tnrs_match_names(names = clean_names, context_name = "All life")
ott_ids <- tx_search$ott_id
lineages <- taxonomy_taxon_info(ott_ids, include_lineage = TRUE)
get_family <- function(info) {
  lineage <- info$lineage
  family <- lineage[sapply(lineage, function(x) x$rank == "family")]
  if (length(family) == 1) return(family[[1]]$name)
  return(NA)
}
circles_prob_dedup$species <- clean_names
families <- sapply(lineages, get_family)
names(families) <- tx_search$unique_name
circles_prob_dedup$family <- families[circles_prob_dedup$species]

# define corner colours
corner_colours <- matrix(c(
  0.0, 0.0, 1.0,   # low-low btmL Blue
  1.0, 0.0, 0.0,   # high-high tpR Red
  0.0, 1.0, 0.0,   # low-high tpL Green
  1.0, 1.0, 0.0    # high-low btmR Yellow
), ncol = 3, byrow = TRUE)

# interpolate for smooth between corner values
interp_colour <- function(a, b, corners) {
  w00 <- (1 - a) * (1 - b)
  w10 <- a * (1 - b)
  w01 <- (1 - a) * b
  w11 <- a * b
  
  rgb_vals <- w00 * corners[1, ] +
    w10 * corners[4, ] +
    w01 * corners[3, ] +
    w11 * corners[2, ]
  print(rgb_vals)
  rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3])
}

circles_prob_dedup$fill_colour <- mapply(interp_colour, a, b, MoreArgs=list(corner_colours))
circles_prob_dedup$alt_fill_colour <- mapply(interp_colour, a, b, MoreArgs=list(alt_corner_colours))
circles_prob_dedup$noncb_fill_colour <- mapply(interp_colour, a, b, MoreArgs=list(noncb_corner_colours))
circles_prob_dedup$fill_colour <- as.character(circles_prob_dedup$fill_colour)
# numbers for writing
percent_both_below_50 <- nrow(circles_prob_dedup[circles_prob_dedup$CNN <= 0.5 & circles_prob_dedup$GPT <= 0.5,]) / nrow(circles_prob_dedup)
percent_both_below_50
nrow(circles_prob_dedup[circles_prob_dedup$CNN < 0.5,]) / nrow(circles_prob_dedup)
nrow(circles_prob_dedup[circles_prob_dedup$CNN > 0.75,]) / nrow(circles_prob_dedup)
nrow(circles_prob_dedup[circles_prob_dedup$GPT > 0.75,]) / nrow(circles_prob_dedup)
nrow(circles_prob_dedup[circles_prob_dedup$CNN > 0.75 | circles_prob_dedup$GPT > 0.75 ,]) / nrow(circles_prob_dedup)
nrow(circles_prob_dedup[circles_prob_dedup$GPT < 0.5,]) / nrow(circles_prob_dedup)
percent_human <- nrow(circles_prob_dedup[circles_prob_dedup$CNN >= 0.90 | circles_prob_dedup$GPT >= 0.90,]) / nrow(circles_prob_dedup)
percent_human
nrow(circles_prob_dedup[circles_prob_dedup$CNN > 0.80,]) / nrow(circles_prob_dedup) * 100
nrow(circles_prob_dedup[circles_prob_dedup$GPT > 0.70,]) / nrow(circles_prob_dedup) * 100
# make tree heatmap
p = gheatmap(p, circles_prob_dedup['fill_colour'], width = 0.4, offset = 0.05, colnames_angle=90, colnames=F, color = NA) +
  scale_fill_identity() +
  # ggtitle("Predicted Performance of Convolutional Neural Networks and ChatGPT across the Mammalian Tree of Life") +
  theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))
#  scale_fill_viridis_c(name = "Probability of correct classification")
p

# make a legend of the interpolated colour map
n <- 50
legend_grid <- expand.grid(a = seq(0, 1, length.out = n),
                           b = seq(0, 1, length.out = n))
legend_grid$fill <- mapply(interp_colour, legend_grid$a, legend_grid$b,
                           MoreArgs = list(corners = corner_colours))
legend_plot <- ggplot(legend_grid, aes(x = a, y = b, fill = fill)) +
  geom_raster() +
  scale_fill_identity() +
  scale_x_continuous(breaks=range(legend_grid$a))+
  scale_y_continuous(breaks=range(legend_grid$b))+
  coord_fixed() +
  labs(title="Probability of Correct \n Classification") +
  xlab("CNN") +
  ylab("GPT") +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 70, face = "bold"),
    axis.title.y = element_text(size = 70, face = "bold", vjust = -1),
    axis.text = element_text(size = 70, face = "bold", colour = "black"),
    plot.title = element_text(size = 70, face = "bold", colour = "black", hjust=1),
    plot.margin = unit(c(0, 0, 0, 0), "cm"))
    # axis.ticks = element_blank())
  # ) +
  # annotate("text", x = 0, y = 0, label = "atop(bold('0'))", hjust = -0.0, vjust = 0.5, parse = TRUE, size = 6) +
  # annotate("text", x = 1, y = 0, label = "atop(bold('1'))", hjust = 1, vjust = 0.5, parse = TRUE, size = 6) +
  # annotate("text", x = 0, y = 1, label = "atop(bold('1'))", hjust = -0.0, vjust = 1, parse = TRUE, size = 6) # +
  # annotate("text", x = 1, y = 1, label = "atop(bold('1,1'))", hjust = 1, vjust = 1, parse = TRUE, size = 6)

legend_plot # fix the positioning of the corner labels, maybe make them short labels only? 
combined_plot <- plot_grid(p, NULL, legend_plot, nrow = 1, rel_widths = c(1, -0.1, 0.25))
combined_plot
ggsave(combined_plot, filename="../results/final/results/circle_final_plot.pdf", device="pdf", units="in", width=50, height=50, limitsize = FALSE)


# ok now doing scatter plot not stacked bars 
averages <- circles_prob_dedup %>%
  group_by(family) %>%
  summarise(
    count = n(),
    avg_CNN = median(CNN, na.rm = TRUE),
    avg_GPT = median(GPT, na.rm = TRUE)
  )

averages$log_gpt <- log(averages$avg_GPT)
averages$log_cnn <- log(averages$avg_CNN)

# Print the result
print(averages)
unique(averages$family)
library(ggrepel)
points_avg_probabilities_plt <- ggplot(data = averages, aes(x = avg_GPT, y = avg_CNN, label = family, size = (count)))+
  geom_point(color='black')+
  # geom_text()+
  ylim(0.4,0.9)+
  xlim(0.05,0.8)+
  # scale_colour_viridis_c()+
  geom_text_repel(aes(label = family),
                  box.padding   = 0.0,
                  point.padding = 0.01,
                  max.overlaps = 10000,
                  size = 4,
                  segment.color = 'grey50') + 
  theme(plot.title = element_text(hjust = 0.5, size = 17), 
        axis.text = element_text(size = 16), 
        axis.title = element_text(size = 18),
        legend.text=element_text(size = 15),
        legend.title = element_text(size=18)) +
  # ggtitle("Median Per Family Correct Classification Probability of both ChatGPT and CNNs in \n N=101 Mammal Families") +
  ylab("Median per family probability of correct classification for CNN") +
  xlab("Median per family probability of correct classification for ChatGPT")
points_avg_probabilities_plt <- points_avg_probabilities_plt + theme(panel.background = element_blank(),
                                                                     panel.border = element_rect(linewidth = 1, fill = NA),
                                                                     panel.grid = element_blank(),
                                                                     axis.text = element_text(colour = "black", size=20, face="bold"),
                                                                     axis.title = element_text(colour = "black", size=20, face="bold"),
                                                                     legend.text = element_text(size=15, face="bold"),
                                                                     legend.title = element_text(size=15, face="bold"),
                                                                     aspect.ratio = 1)
points_avg_probabilities_plt <- points_avg_probabilities_plt + labs(size='Number of species') 
points_avg_probabilities_plt
ggsave(points_avg_probabilities_plt, filename="../results/final/results/points_probabilities_plot_filtered.pdf", device="pdf", units="in", width=12, height=12, limitsize = FALSE)

# make histogram of per family average accuracy observed vs imputed
# Interleaved histograms
# observed_species_level <- scaled_phylo_df %>% filter(!is.na(correct_or_not_gpt.mean))
spec_df_obs <- scaled_phylo_df %>% filter(!is.na(correct_or_not_gpt.mean))
spec_df_obs = unique(spec_df_obs[,c("sci_name", "tips_chr", "body_log", "range_log", "iucn_category", "family", "order", "class")])
base_df = base_df[rep(seq_len(nrow(base_df)), each = nrow(spec_df_obs)), ]
spec_df_obs = cbind(spec_df_obs, base_df)
spec_cnn_obs = posterior_epred(m_cnn, newdata = spec_df_obs, ndraws = 1000, re_formula = ~ (1|sci_name) + (1|gr(tips_chr, cov = A)), allow_new_levels = TRUE)
spec_gpt_obs = posterior_epred(m_gpt, newdata = spec_df_obs, ndraws = 1000, re_formula =  ~ (1|sci_name) + (1|gr(tips_chr, cov = A)), allow_new_levels = TRUE)
spec_cnn_obs = cbind(data.frame(samples = c(1:1000)), spec_cnn_obs)
spec_cnn_obs = spec_cnn_obs %>%
  pivot_longer(!samples, names_to = "var", values_to = "val") %>%
  left_join(data.frame(var = as.character(c(1: nrow(spec_df_obs))), code = spec_df_obs[["sci_name"]]))
sum_spec_cnn_obs = spec_cnn_obs %>%
  group_by(code) %>%
  dplyr::summarise(cnn_prob = mean(val))
spec_gpt_obs = cbind(data.frame(samples = c(1:1000)), spec_gpt_obs)
spec_gpt_obs = spec_gpt_obs %>%
  pivot_longer(!samples, names_to = "var", values_to = "val") %>%
  left_join(data.frame(var = as.character(c(1: nrow(spec_df))), code = spec_df[["sci_name"]]))
sum_spec_gpt = spec_gpt_obs %>%
  group_by(code) %>%
  dplyr::summarise(gpt_prob = mean(val))
sum_spec_obs = left_join(sum_spec_cnn_obs, sum_spec_gpt)
circles_prob_obs = data.frame(
  obs_cnn = sum_spec_obs$cnn_prob,
  obs_gpt = sum_spec_obs$gpt_prob,
  species = sum_spec_obs$code
)
observed_species_level <- circles_prob_obs[unique_indices, , drop = FALSE]



rownames(circles_prob_dedup) <- NULL 
imputed_spp_level <- circles_prob_dedup %>% select(CNN, GPT, species, family)
imputed_spp_level <- imputed_spp_level %>% rename(imp_gpt = GPT, imp_cnn = CNN)
imputed_spp_level <- imputed_spp_level %>% rename(imp_gpt = GPT, imp_cnn = CNN)
observed_species_level <- observed_species_level %>% rename(obs_cnn = correct_cnn.mean, obs_gpt = correct_or_not_gpt.mean, species = sci_name)
# mask out families common to both analyses
histo_compare_df <- left_join(observed_species_level, imputed_spp_level, by="species") # rename cols to obs_gpt and imp_gpt
histo_compare_df <- histo_compare_df %>% select(species,obs_gpt,obs_cnn,imp_cnn,imp_gpt)
histo_compare_df <- histo_compare_df[complete.cases(histo_compare_df), ]
# Convert to long format
long_df <- histo_compare_df %>%
  pivot_longer(
    cols = c(obs_gpt, imp_gpt, obs_cnn, imp_cnn),
    names_to = c("type", "model"),
    names_sep = "_",
    values_to = "value"
  )
long_df$model[long_df$model=='gpt'] <- 'GPT'
long_df$model[long_df$model=='cnn'] <- 'CNN'
histogram_plot <- ggplot(long_df, aes(x = value, fill = type)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  facet_wrap(~ model, scales = "fixed") +
  scale_fill_manual(labels = c("Imputed", "Observed"), values = c("obs" = "#E69F00", "imp" = "#56B4E9")) +
  theme_minimal() +
  theme(legend.position = "top") +
  # labs(title = "Observed vs Imputed Distributions in N=115 Species", x = "Probability of Correct Classificaiton", y = "Count", fill = "Type")
  labs(x = "Probability of Correct Classification", y = "Number of Species", fill = "Type") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(linewidth = 1, fill = NA),
        panel.grid = element_blank(),
        axis.text = element_text(colour = "black", size=10),
        aspect.ratio = 1,
        axis.title=element_text(size=12))
histogram_plot
ggsave(histogram_plot, filename="../results/final/results/species_histogram.pdf", device="pdf", units="cm", width=12, height=12, limitsize = FALSE)
violin_plot <- ggplot(long_df, aes(x = type, y = value, fill = type)) +
  geom_violin(position = "identity", alpha = 0.5) +
  facet_wrap(~ model, scales = "fixed") +
  scale_fill_manual(labels = c("Imputed", "Observed"), values = c("obs" = "#E69F00", "imp" = "#56B4E9")) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(x = "Type", y = "Probability of Correct Classification", fill = "Type")  +
  theme(panel.background = element_blank(),
        panel.border = element_rect(linewidth = 1, fill = NA),
        panel.grid = element_blank(),
        axis.text = element_text(colour = "black", size=10),
        aspect.ratio = 1,
        axis.title=element_text(size=12))
violin_plot
ggsave(violin_plot, filename="../results/final/results/species_violin.pdf", device="pdf", units="cm", width=12, height=12, limitsize = FALSE)
density_plot <- ggplot(long_df, aes(x = value, fill = type)) +
  geom_density(position = "identity", alpha = 0.5, bins = 30) +
  facet_wrap(~ model, scales = "fixed") +
  scale_fill_manual(labels = c("Imputed", "Observed"), values = c("obs" = "#E69F00", "imp" = "#56B4E9")) +
  theme_minimal() +
  theme(legend.position = "top") +
  # labs(title = "Observed vs Imputed Distributions in N=115 Species", x = "Probability of Correct Classificaiton", y = "Count", fill = "Type")
  labs(x = "Probability of Correct Classification", y="Density", fill = "Type")  +
  theme(panel.background = element_blank(),
        panel.border = element_rect(linewidth = 1, fill = NA),
        panel.grid = element_blank(),
        axis.text = element_text(colour = "black", size=10),
        aspect.ratio = 1,
        axis.title=element_text(size=12))
density_plot
ggsave(density_plot, filename="../results/final/results/species_density.pdf", device="pdf", units="cm", width=12, height=12, limitsize = FALSE)

# phylogenetic signal taken from https://rdrr.io/cran/brms/f/vignettes/brms_phylogenetics.Rmd 
# The so called phylogenetic signal (often symbolize by $\lambda$) can be computed with the hypothesis method and is roughly $\lambda = 0.7$ for this example.

hyp <- "sd_tips_char__Intercept^2 / (sd_tips_char__Intercept^2 + sigma^2) = 0"
(hyp <- hypothesis(m_cnn, hyp, class = NULL))
plot(hyp)

# Note that the phylogenetic signal is just a synonym of the intra-class correlation (ICC) used in the context phylogenetic analysis.
# or is it from https://discourse.mc-stan.org/t/phylogenetic-signal-for-bernoulli-families-by-brms/19608/13
lambda = sd_phylo^2 / (sd_phylo^2 + sd_sci_name^2)

# or https://groups.google.com/g/brms-users/c/8ADbWh7v9kc says because bernoulli is like that, we need to replace the sigma term with a constant, e.g. You either have to ignore it in the computation of the phylogenetic signal or replace it by some fixed value (pi^2 / 3 I believe. This is the variance of the standard logistic distribution). 
hyp <- "sd_tips_chr__Intercept^2 / (sd_tips_chr__Intercept^2 + 3.289868) = 0" # pi^2/3 is 3.289.... possibly want to find the variance of my distribution but it'll do for now
(hyp <- hypothesis(imputed_m_cnn, hyp, class = NULL))
plot(hyp)
