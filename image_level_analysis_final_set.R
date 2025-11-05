library(dplyr)
library(readxl)
library(lme4)
library(lmerTest)
library(tidyr)

setwd("code")

# need to change any invasive stats = yes to invasive

df.image_level <- read_xlsx("../data/data_for_tom_inc_month 1 (2).xlsx")
names(df.image_level)[names(df.image_level) == "dataset_id"] <- "location"
# df_zimbabwe <- df.image_level %>% filter(location == "Zimbabwe(matt)")
df.image_level <- df.image_level %>% filter(location != "Zimbabwe(matt)")

df_class_level_nonsense <- read.csv("../data/GPT_class_level_data_unifed_names.csv")
df_zimbabwe <- df_class_level_nonsense %>% filter(dataset == "zimbabwe")
df_zimbabwe <- df_zimbabwe[,c("species","percent_correct","dataset")]
# df_serengeti <- df_class_level_nonsense %>% filter(dataset == "serengeti")


df_class_level_nonsense <- df_class_level_nonsense %>% filter(dataset != "zimbabwe")

length(unique(df_class_level_nonsense$species))
unique(df_class_level_nonsense$species)

length(unique(df.image_level$sci_name))
unique(df.image_level$sci_name)

length(unique(df.image_level$common_name))
unique(df.image_level$common_name)

df_original_image_level <- read.csv('../data/image_level_data.csv')
label_replacements <- c(
  "greater mouse-deer" = "greater mousedeer",
  "lesser mouse-deer" = "lesser mousedeer",
  "Sunda leopard cat" = "sunda leopard cat",   
  "Sunda pangolin" = "sunda pangolin",
  "pig-tailed macaque" = "pigtailed macaque", 
  "Long-footed Treeshrew" = "longfooted treeshrew",
  "short-tailed mongoose" = "shorttailed mongoose",
  "yellow-throated marten" = "yellowthroated marten",
  "Asiatic elephant" = "asiatic elephant",
  "black-tailed jackrabbit" = "blacktailed jackrabbit",
  "yellow-bellied marmot" = "yellowbellied marmot",
  "North American Badger" = "north american badger",
  "long-tailed weasel" = "longtailed weasel",
  "nine-banded armadillo" = "ninebanded armadillo",
  "dog" = "domestic dog",
  "cattle" = "domestic cattle"
  # Add more replacements here
)

df_original_image_level <- df_original_image_level %>%
  mutate(correct_label = recode(correct_label, !!!label_replacements))

# df_original_image_level[df_original_image_level$correct_label == "greater mouse-deer",] <- "greater mousedeer"
# df_original_image_level[df_original_image_level$correct_label == "lesser mouse-deer",] <- "lesser mousedeer" 
length(unique(df_original_image_level$correct_label))

# make a col that specifies if kept for later or not
df_original_image_level$kept_downstream <- NA
# use df.image_level$common_name I think?
for (i in 1:nrow(df_original_image_level)) {
  img_label <- df_original_image_level$correct_label[i]
  if (img_label %in% unique(df.image_level$common_name)) {
    df_original_image_level$kept_downstream[i] <- TRUE
  } else {
    df_original_image_level$kept_downstream[i] <- FALSE
  }
}
unique(df_original_image_level$location)
df_original_image_level$location[df_original_image_level$location == "Zimbabwe(matt)"] <- "zimbabwe"
df_zimbabwe <- df_original_image_level %>% filter(location == "zimbabwe")
unique(df_zimbabwe$correct_label)
df_original_image_level <- df_original_image_level %>% filter(location != "zimbabwe")

df_domestics <- df_original_image_level %>%
  filter(correct_label %in% c("domestic dog", "domestic cattle", "goat", "pig", "cat", "domestic horse"))


# df_original_image_level_dog <- df_original_image_level %>% filter(correct_label == "domestic dog")
# df_original_image_level_cow <- df_original_image_level %>% filter(correct_label == "domestic cattle")
# df_original_image_level_goat <- df_original_image_level %>% filter(correct_label == "goat")
# df_original_image_level_pig <- df_original_image_level %>% filter(correct_label == "pig")
# df_original_image_level_cat <- df_original_image_level %>% filter(correct_label == "cat")
# df_original_image_level_horse <- df_original_image_level %>% filter(correct_label == "domestic horse")
# 708 total images for domestics

labels_original <- unique(df_original_image_level$correct_label)
labels_new <- unique(df.image_level$common_name)
setdiff(labels_original, labels_new)
setdiff(labels_new, labels_original)



vague_labels <- c(
  "crane", "cuckoo", "dove", "landfowl", "langur", "songbird", "civet", "duiker", 
  "genet", "guineafowl", "hare", "jackal", "mongoose", "other bird", "porcupine", 
  "reedbuck", "rodents", "vulture", "bird", "deer", "mouse", "mustelid",
  "ants","margarita island capuchin", "rodent"
)

df_original_image_level <- df_original_image_level %>%
  filter(!correct_label %in% vague_labels)

unique(df_original_image_level$correct_label)
colnames(df.image_level)
colnames(df_original_image_level)
# remove the multiple bbox images....
df_original_image_level_no_mulitple_boxes <- df_original_image_level %>% filter(multiple_boxes == 0)
# 8979 from some later merging, 10246 without removing multiple box images, 8442 without multiple boxes, god I hate having to do forensic work on my own data 
df_original_image_level <- df_original_image_level %>% filter(multiple_boxes == 0)
df_original_image_level <- df_original_image_level %>% select(-blurry, -percent_white, -flash_fired, -multiple_boxes_proportion)
df_original_image_level <- df_original_image_level %>%
  rename(common_name = correct_label)


key_cols <- c("correct_or_not_gpt","correct_cnn", "common_name", "location", "img_size_px", "bbox_size_px", "blur_value", "isNight", "isGreyScale", "bbox_proportion", "bbox_touches_edge")


# Full outer join on key columns, bring in all other columns
df_diff_full <- full_join(
  df_original_image_level,
  df.image_level,
  by = key_cols,
  suffix = c("_orig", "_new")
)


df_only_in_original <- anti_join(df_original_image_level, df.image_level, by = key_cols)
df_only_in_new <- anti_join(df.image_level, df_original_image_level, by = key_cols)

# Columns to join by
join_cols <- c("correct_or_not_gpt", "common_name", "correct_cnn", "isNight",
               "img_size_px", "bbox_size_px", "bbox_proportion", "bbox_touches_edge",
               "isGreyScale", "blur_value", "location")

# Join the two dataframes
df_joined <- full_join(df_original_image_level, df.image_level, by = join_cols)
columns_to_fill <- c("sci_name", "body_mass_kg", "range_area_km", 
                     "gbif_taxonID", "genus", "family", "order",
                     "iucn_category", "IUCN_sci_name", "dataset", "invasive_status", "locations_appearing_in")

df_filled <- df_joined %>%
  group_by(common_name) %>%
  fill(all_of(columns_to_fill), .direction = "downup") %>%
  ungroup()

write.csv(df_filled, "../data/full_images_final_analysis_v5.csv", row.names = FALSE)



# read back in the updated csv, that had the domestics mostly filled in? 
df_man_poped <- read.csv('../data/full_images_final_analysis_v4.csv')

df_merged <- full_join(
  df_filled, df_man_poped,
  by = key_cols,
  suffix = c("_main", "_data")
)

all_cols <- colnames(df_merged)
main_cols <- all_cols[grepl("_main$", all_cols)]
data_cols <- all_cols[grepl("_data$", all_cols)]
shared_bases <- intersect(
  sub("_main$", "", main_cols),
  sub("_data$", "", data_cols)
)

# Step 4: Coalesce with safe coercion
for (col in shared_bases) {
  main_col <- paste0(col, "_main")
  data_col <- paste0(col, "_data")
  
  # Check for compatible types and coerce to character if they differ
  if (class(df_merged[[main_col]]) != class(df_merged[[data_col]])) {
    df_merged[[col]] <- coalesce(
      as.character(df_merged[[data_col]]),
      as.character(df_merged[[main_col]])
    )
  } else {
    df_merged[[col]] <- coalesce(df_merged[[data_col]], df_merged[[main_col]])
  }
}

# Step 5: Drop old _main and _data columns
drop_cols <- c(paste0(shared_bases, "_main"), paste0(shared_bases, "_data"))
df_filled <- df_merged %>% select(-all_of(drop_cols))

# write.csv(df_filled, "../data/final_image_level_datav5.csv", row.names = FALSE)


# df$dataset[df$dataset == "boreno"] <- "borneo"
# write.csv(df, "../data/final_image_level_data.csv")

# non_key_cols <- intersect(
#   setdiff(names(df_main), key_cols),
#   setdiff(names(df_data), key_cols)
# )
# df_filled_final <- df_merged %>%
#   mutate(across(
#     .cols = all_of(non_key_cols),
#     .fns = ~coalesce(
#       get(paste0(cur_column(), "_data")),
#       get(paste0(cur_column(), "_main"))
#     ),
#     .names = "{.col}"
#   )) %>%
#   select(all_of(key_cols), all_of(non_key_cols))

# get range area for domestics
library(rgbif)
library(sf)



get_gbif_ranges <- function(spp_name) {
  # e.g. sus scrofa
  # Query GBIF for species occurrence points (limit to 10k for performance)
  species_name <- spp_name
  occ <- occ_search(scientificName = species_name, limit = 10000, hasCoordinate = TRUE)
  print(gbif_citation(occ))
  # occ$citation
  df_points <- occ$data %>%
    filter(!is.na(decimalLatitude), !is.na(decimalLongitude)) %>%
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
  # Transform to an equal-area projection (important for accurate area)
  df_proj <- st_transform(df_points, crs = "+proj=aea +lat_1=10 +lat_2=40 +lat_0=0 +lon_0=0")
  # Create a convex hull
  hull <- st_convex_hull(st_union(df_proj))
  area_km2 <- st_area(hull) %>% units::set_units(km^2)
  print(area_km2)
}

get_gbif_ranges("sus scrofa")
get_gbif_ranges("felis catus")
get_gbif_ranges("canis familiaris")
get_gbif_ranges("equus caballus")
get_gbif_ranges("capra hircus")
get_gbif_ranges("bos taurus")

df_filled <- read.csv("../data/full_images_final_analysis_v2.csv")
df_filled_no_na <- df_filled %>% filter(!is.na(isNight))

df_filled_no_domestics <- df_filled %>%
  filter(!common_name %in% c("pig", "domestic dog", "domestic horse", "cat", "goat", "domestic cattle"))


unique(df.image_level$common_name)
unique(df_filled_no_domestics$common_name)

df_with_na <- df_filled_no_domestics %>%
  filter(if_any(everything(), is.na))
no_isNight_na <- df_filled_no_domestics %>% filter(is.na(isNight))

# cast df_filled to df.image_level as that is now the final image level data
df.image_level <- df_filled








df.image_level$bbox_touches_edge <- as.logical(df.image_level$bbox_touches_edge)
df.rescaled <- df.image_level

# df.rescaled[c(5:7,10,14,15)] <- lapply(df.rescaled[c(5:7,10,14,15)], function(x) c(scale(x)))
cols_to_scale <- c("img_size_px", "bbox_size_px", "bbox_proportion", "blur_value", "body_mass_kg", "range_area_km")

df.rescaled[cols_to_scale] <- lapply(df.rescaled[cols_to_scale], function(x) c(scale(x)))

write.csv(df.rescaled, "../data/final_image_level_data_rescaledv2.csv", row.names = FALSE)
length(unique(df.rescaled$common_name))

model_image_level_gpt <- glmer(correct_or_not_gpt ~ (1 | sci_name) +
                             bbox_size_px +
                             bbox_touches_edge +
                             isNight + isGreyScale +
                             blur_value + (1|location), 
                           data = df.rescaled, family = binomial)
coef(summary(model_image_level_gpt))
summary(model_image_level_gpt)

model_image_level_cnn <- glmer(correct_cnn ~ (1 | sci_name) +
                                 bbox_size_px +
                                 bbox_touches_edge +
                                 isNight + isGreyScale +
                                 blur_value + (1|location), 
                               data = df.rescaled, family = binomial)
coef(summary(model_image_level_cnn))
summary(model_image_level_cnn)

df.image_level <- read.csv("../data/final_image_level_data_v6.csv")
df.image_level$correct_cnn[is.na(df.image_level$correct_cnn)] <- 0

num_images <- nrow(df_zimbabwe)
percent_image_coverage_gpt <- df_zimbabwe %>% filter(correct_or_not_gpt == TRUE)
percent_image_coverage_gpt <- nrow(percent_image_coverage_gpt) / nrow(df_zimbabwe)
percent_image_coverage_gpt

# how much only gpt gets correct
num_images <- nrow(df.image_level)
gpt_only_correct <- df.image_level %>% 
  filter(correct_or_not_gpt == TRUE & correct_cnn == FALSE)
num_gpt_correct <- nrow(gpt_only_correct)
# how much only cnn get correct
cnn_only_correct <- df.image_level %>% 
  filter(correct_or_not_gpt == FALSE & correct_cnn == TRUE)
num_cnn_correct <- nrow(cnn_only_correct)
both_incorrect <- df.image_level %>% 
  filter(correct_or_not_gpt == FALSE & correct_cnn == FALSE)
num_both_incorrect <- nrow(both_incorrect)
both_correct <- df.image_level %>% 
  filter(correct_or_not_gpt == TRUE & correct_cnn == TRUE)
num_both_correct <- nrow(both_correct)

percent_image_coverage_gpt <- df.image_level %>% filter(correct_or_not_gpt == TRUE)
percent_image_coverage_gpt <- nrow(percent_image_coverage_gpt) / nrow(df.image_level)
percent_image_coverage_gpt

percent_image_coverage_cnn <- df.image_level %>% filter(correct_cnn == TRUE)
percent_image_coverage_cnn <- nrow(percent_image_coverage_cnn) / nrow(df.image_level)
percent_image_coverage_cnn



get_image_coverage <- function(df, name) {
  percent_image_coverage_gpt <- df %>% filter(correct_or_not_gpt == TRUE)
  percent_image_coverage_gpt <- nrow(percent_image_coverage_gpt) / nrow(df)
  percent_image_coverage_gpt
  
  percent_image_coverage_cnn <- df %>% filter(correct_cnn == TRUE)
  percent_image_coverage_cnn <- nrow(percent_image_coverage_cnn) / nrow(df)
  percent_image_coverage_cnn
  
  paste0("For dataset ", name, " percent image coverage for gpt is: ", percent_image_coverage_gpt, ", and cnn is: ", percent_image_coverage_cnn)
}

# dataset level metrics? 
borneo_df <- df.image_level %>% filter(location == "Borneo")
get_image_coverage(borneo_df, "Borneo")
orinoquia_df <- df.image_level %>% filter(location == "Orinoquia")
get_image_coverage(orinoquia_df, "Orinoquia")
north_am_df <- df.image_level %>% filter(location == "North_America")
get_image_coverage(north_am_df, "North America")
serengeti_df <- df.image_level %>% filter(location == "Serengeti")
get_image_coverage(serengeti_df, "Serengeti")
wellington_df <- df.image_level %>% filter(location == "Wellington")
get_image_coverage(wellington_df, "Wellington")

serengeti_df <- serengeti_df %>%
  group_by(common_name, location) %>%
  dplyr::summarise(gpt_accuracy = mean(correct_or_not_gpt),
                   cnn_accuracy = mean(correct_cnn))

borneo_class_level <- borneo_df %>%
  group_by(common_name, location) %>%
  dplyr::summarise(gpt_accuracy = mean(correct_or_not_gpt),
                   cnn_accuracy = mean(correct_cnn))

cols <- c("correct_or_not_gpt", "correct_cnn")
df.image_level[,cols] <- lapply(df.image_level[,cols], as.numeric)
df.class_level <- df.image_level %>%
  group_by(common_name, location) %>%
  dplyr::summarise(gpt_accuracy = mean(correct_or_not_gpt),
                   cnn_accuracy = mean(correct_cnn))

get_mean_median_acc <- function(df, name) {
  mean_gpt <- mean(df$gpt_accuracy)
  mean_cnn <- mean(df$cnn_accuracy)
  median_gpt <- median(df$gpt_accuracy)
  median_cnn <- median(df$cnn_accuracy)
  
  return(c(
    # paste0("For dataset ", name, " per class mean gpt accuracy is: ", mean_gpt, " and cnn is: ", mean_cnn), 
    paste0("For dataset ", name, " per class median gpt accuracy is: ", median_gpt, " and cnn is: ", median_cnn)
  ))
  
}

grouped_summaries <- df.class_level %>%
  group_by(location) %>%
  group_split() %>%
  lapply(function(group) {
    loc_name <- unique(group$location)
    get_mean_median_acc(group, loc_name)
  })
# Flatten and name the results
names(grouped_summaries) <- sapply(df.class_level %>% group_by(location) %>% group_keys() %>% pull(), as.character)
unlist(grouped_summaries)

get_mean_median_acc(df.class_level, "All data")
df_class_minus_borneo <- df.class_level %>% filter(location != "Borneo")
get_mean_median_acc(df_class_minus_borneo, "without borneo")
# proportions
prop_gpt <- num_gpt_correct / num_images
prop_cnn <- num_cnn_correct / num_images
prop_incorrect <- num_both_incorrect / num_images
prop_correct <- num_both_correct / num_images
prop_gpt
prop_cnn
prop_incorrect
prop_correct
# now need to do class level summaries
# gpt_mean = mean(df.image_level$correct_or_not_gpt)
# gpt_median = median(df.image_level$correct_or_not_gpt)
# cast correct_or_not_gpt and correct_cnn to numeric

df.class_level$delta <- df.class_level$gpt_accuracy - df.class_level$cnn_accuracy

median(df.class_level$delta)
mean(df.class_level$delta)
df.class_level$gpt_better = df.class_level$gpt_accuracy > df.class_level$cnn_accuracy
df.class_level$cnn_better = df.class_level$cnn_accuracy > df.class_level$gpt_accuracy
df.class_level$gpt_better_or_equal = df.class_level$gpt_accuracy >= df.class_level$cnn_accuracy
df.class_level$cnn_better_or_equal = df.class_level$cnn_accuracy >= df.class_level$gpt_accuracy
sum(df.class_level$gpt_better) / nrow(df.class_level)
sum(df.class_level$cnn_better) / nrow(df.class_level)
sum(df.class_level$gpt_better_or_equal) / nrow(df.class_level)
sum(df.class_level$cnn_better_or_equal) / nrow(df.class_level)

# proportion they both get wrong
both_incorrect_class <- df.class_level %>% 
  filter(gpt_accuracy == 0 & cnn_accuracy == 0)
num_both_incorrect_class <- nrow(both_incorrect_class)
num_both_incorrect_class / nrow(df.class_level)
both_correct_class <- df.class_level %>% 
  filter(gpt_accuracy > 0 & cnn_accuracy > 0)
num_both_correct_class <- nrow(both_correct_class)
num_both_correct_class / nrow(df.class_level)

same_accuracy_class <- df.class_level %>% 
  filter(delta == 0)
num_same_accuracy_class <- nrow(same_accuracy_class)
num_same_accuracy_class / nrow(df.class_level)

# delta when better
delta_gpt_better <- df.class_level %>% 
  filter(gpt_better == TRUE)
mean(delta_gpt_better$delta)
delta_cnn_better <- df.class_level %>% 
  filter(cnn_better == TRUE)
delta_cnn_better$cnn_delta <- delta_cnn_better$cnn_accuracy - delta_cnn_better$gpt_accuracy
mean(delta_cnn_better$cnn_delta)

summary_stats_whole_data <- df.image_level %>%
  summarise(
    mean_accuracy_cnn = mean(accuracy),
    median_accuracy_cnn = median(accuracy),
    mean_acc_gpt = mean(percent_correct),
    median_acc_gpt = median(percent_correct),
    percent_gpt_better = sum(GPTBetter == 1) / n(),
    percent_gpt_better_or_equal = sum(GPTBetterOREqual == 1) / n(),
    percent_cnn_better = sum(GPTBetter == 0) / n(),
    percent_cnn_better_or_equal = sum(GPTBetterOREqual == 0) / n(),
    mean_delta_gpt_better = mean(delta[GPTBetter == 1], na.rm = TRUE),
    median_delta_gpt_better = median(delta[GPTBetter == 1], na.rm = TRUE),
    mean_delta_gpt_better_or_equal = mean(delta[GPTBetterOREqual == 1], na.rm = TRUE),
    median_delta_gpt_better_or_equal = median(delta[GPTBetterOREqual == 1], na.rm = TRUE),
    mean_delta_cnn_better = mean(cnn_delta[GPTBetter == 0], na.rm = TRUE),
    median_delta_cnn_better = median(cnn_delta[GPTBetter == 0], na.rm = TRUE),
    mean_delta_cnn_better_or_equal = mean(cnn_delta[GPTBetterOREqual == 0], na.rm = TRUE),
    median_delta_cnn_better_or_equal = median(cnn_delta[GPTBetterOREqual == 0], na.rm = TRUE)
  )


length(unique(borneo_df$sci_name))
length(unique(orinoquia_df$sci_name))
length(unique(north_am_df$sci_name))
length(unique(serengeti_df$common_name))
length(unique(wellington_df$sci_name))

df_orig <- read.csv('../data/allMergedData.csv')
length(unique(df_orig$correct_label))
df_compare <- read_xlsx("../data/data_for_tom_inc_month 1 (2).xlsx")
length(unique(df_compare$common_name))
spp_left <- unique(df_compare$common_name)
df_orig_names_removed <- df_orig %>% filter(correct_label %in% gsub(" ", "", spp_left))
unique(df_orig_names_removed$correct_label)
df_orig$bbox_size_px <- as.numeric(sapply(strsplit(df_orig$bbox_size_px, ","), function(x) trimws(x[1])))
df_orig$bbox_touches_edge <- as.numeric(sapply(strsplit(df_orig$bbox_touches_edge, ","), function(x) trimws(x[1])))

df_orig$bbox_touches_edge
# df_orig$bbox_size_px 


df_merged <- inner_join(
  df_orig,
  df_compare,
  by = c("bbox_size_px", "isGreyScale", "blur_value")
)
df_unmatched <- anti_join(
  df_compare,
  df_merged,
  by = c("bbox_size_px", "isGreyScale", "blur_value")
)


