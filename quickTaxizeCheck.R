library(taxize)
library(dplyr)
setwd("code")
df <- read.csv("../data/all_mammals_range_trait.csv")
missing_bodysize <- df %>% filter(if_any(body_mass_kg, is.na))
for (row in 1:nrow(missing_bodysize)) {
  name <- missing_bodysize$sci_name[row]
  # get taxise names 
  resolved_names <- gnr_resolve(name)
  print(resolved_names)
  # maybe put it back in? or do we want to just do this interactively?
  
}

# ok so the above has the most up to date names it seems, so will have to get this list from the trait db, update the names and rematch on there

trait_data <- read.csv("../data/trait_data_reported.csv")

# get iucn2020_binomial, take the best match taxise has
for (row in 1:nrow(trait_data)) {
  name <- trait_data$iucn2020_binomial[row]
  # get taxise names 
  resolved_names <- gnr_resolve(name)
  top_match <- resolved_names[which.max(resolved_names$score), ]
  top_match <- top_match$matched_name
  trait_data$resolved_name[row] <- top_match 
  # print(top_match)
  # print(resolved_names)
  # maybe put it back in? or do we want to just do this interactively?
  
}
print(colnames(trait_data))
# write it out for safety
write.csv(trait_data, "../data/trait_data_resolved_names.csv")
# try again
trait_data <- read.csv("../data/trait_data_resolved_names.csv")
names(trait_data)[names(trait_data) == 'resolved_name'] <- 'sci_name'
toKeep <- c('sci_name', 'iucn2020_binomial', 'adult_mass_g')
trait_data <- trait_data[toKeep]
trait_data$body_mass_kg <- trait_data$adult_mass_g / 1000
trait_data <- trait_data[,!colnames(trait_data) %in% c('adult_mass_g')]
trait_data <- trait_data[(trait_data$sci_name %in% missing_bodysize$sci_name),]


trait_data_species <- trait_data$sci_name
missing_bodysize$sci_name[!(missing_bodysize$sci_name %in% trait_data_species)]


missing_bodysize <- missing_bodysize %>%
  left_join(trait_data, by = "sci_name", relationship = "many-to-many")

length(missing_bodysize$sci_name) - length(unique(missing_bodysize$sci_name))
length(missing_bodysize$sci_name)
length(missing_bodysize$body_mass_kg[missing_bodysize$body_mass_kg == "NA"])