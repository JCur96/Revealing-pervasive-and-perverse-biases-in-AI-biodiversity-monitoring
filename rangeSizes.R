library(sf)
library(terra)
library(dplyr)
setwd("code")

# read in the species_names_updated.csv file
# species <- read.csv("../data/species_names_updated.csv")
species <- read.csv("../data/final_species_names.csv")

# change the borneo elephant to asian elephant, should get body mass of 3220000g
species[species$sci_name == "Elephas maximus borneensis", "sci_name"] <- "Elephas maximus"
species
# read in the mammal shape file
mammals <- st_read("../data/IUCN_Maps/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
# mammals <- vect("../data/IUCN_Maps/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
mammals
# names_to_keep <- species$sci_name
# drop things above species level, as in any name without a second word
species_only_binomial <- species[grep(" ", species$sci_name),]
# names_to_keep <- species_only_binomial$sci_name
names_to_keep <- species_only_binomial
# drop all rows that are not in species (sci_name is comparison col for both)
# mammals <- mammals %>% filter(sci_name == species$sci_name)
mammals <- mammals[(mammals$sci_name %in% names_to_keep),]
# drop irrelevant cols
cols_to_keep <- c("id_no", "sci_name", "SHAPE_Leng", "SHAPE_Area", "geometry")
mammals <- mammals[cols_to_keep]
# calculate the size of each range given, add them for each unique species
# validity <- st_is_valid(mammals)
# invalid_geoms <- which(!validity)
# print(mammals[invalid_geoms,])
# units::set_units(mammals$SHAPE_Area, "km^2")
# cast to simple polygons?
test_row <- mammals[1,]
test_area <- st_area(test_row)
test_area <- units::set_units(test_area, "km^2")
test_area
# mammals <- st_make_valid(mammals)
# mammals <- st_cast(mammals, "POLYGON")
mammals <- st_transform(mammals, 3857)
# mammals <- st_transform(mammals, 4326)
area_values <- st_area(mammals$geometry)
area_values <- units::set_units(area_values, "km^2")
mammals$range_area_km <- area_values
# now add range values for each species so that there is one number per species
area_totals <- mammals %>% group_by(id_no) %>% summarize(total_area = sum(range_area_km))
area_totals <- st_drop_geometry(area_totals)
mammals <- mammals %>% left_join(area_totals, by = "id_no")
mammals <- st_drop_geometry(mammals)
# drop id_no, SHAPE_Leng, SHAPE_Area, range_area_km, get unique records
keeps <- c("sci_name", "total_area")
mammals <- mammals[keeps]
mammals <- unique(mammals)
# now to join this back to the species df, despite unequal length 
# species_updated <- full_join(species, mammals, by = "sci_name")
species_updated <- species %>%
  left_join(mammals, by = "sci_name")


# move the range size to a range_area_km col, dropping the units
species_updated$range_area_km <- species_updated$total_area
species_updated$range_area_km <- as.numeric(species_updated$range_area_km)
# drop range_size, body_size and total_area
toDrop <- c('range_size', 'body_size', 'total_area')
species_updated <- species_updated[, !colnames(species_updated) %in% toDrop]
# need to do the same for birds and reptiles

# also do trait data
trait_data <- read.csv("../data/trait_data_reported.csv")
# merge genus and species col to get sci name
# trait_data$sci_name <- paste(trait_data$genus, trait_data$species, sep = ' ')
# keep only sci_name, iucn2020_binomial, adult_mass_g
toKeep <- c('iucn2020_binomial', 'adult_mass_g')
trait_data <- trait_data[toKeep]
# rename iucn2020_binomial to sci_name, convert g into kg
names(trait_data)[names(trait_data) == 'iucn2020_binomial'] <- 'sci_name'
trait_data$body_mass_kg <- trait_data$adult_mass_g / 1000
# filter trait data to just those species we have in our list
trait_data <- trait_data[(trait_data$sci_name %in% names_to_keep),]
# see what we are missing
trait_data_species <- trait_data$sci_name
names_to_keep[!(names_to_keep %in% trait_data_species)]
# drop adult_mass_g
trait_data <- trait_data[,!colnames(trait_data) %in% c('adult_mass_g')]
# merge the mammal trait data back into the species_updated frame
# species_updated <- merge(species_updated, trait_data, by = 'sci_name', all=TRUE)
species_updated <- species_updated %>%
  left_join(trait_data, by = "sci_name")

# now add birds 
bird_traits_and_ranges <- read.csv("../data/AVONET1_BirdLife.csv")
# keep Speices1, Family1, Order1, Mass, Range.Size
toKeep <- c('Species1', 'Mass', 'Range.Size')
bird_traits_and_ranges <- bird_traits_and_ranges[toKeep]
bird_traits_and_ranges <- bird_traits_and_ranges %>% 
  rename(
    sci_name = Species1,
    body_mass_kg = Mass,
    range_area_km = Range.Size
  )
bird_traits_and_ranges$body_mass_kg <- bird_traits_and_ranges$body_mass_kg / 1000
bird_traits_and_ranges <- bird_traits_and_ranges[(bird_traits_and_ranges$sci_name %in% names_to_keep),]
# merge the datasets
# species_updated <- merge(species_updated, bird_traits_and_ranges, all=TRUE)
species_updated <- species_updated %>%
  left_join(bird_traits_and_ranges, by = "sci_name")

species_updated <- species_updated %>%
  mutate(
    body_mass_kg = coalesce(body_mass_kg.x, body_mass_kg.y),
    range_area_km = coalesce(range_area_km.x, range_area_km.y)
  ) %>%
  select(-body_mass_kg.x, -body_mass_kg.y, -range_area_km.x, -range_area_km.y)

species_updated <- species_updated %>%
  distinct(sci_name, .keep_all = TRUE)

# save this out for manaual tweaking because jesus
write.csv(species_updated, "../data/species_trait_and_range.csv", row.names = FALSE)
# read it back in, now with some of the stupid sorted out 

# try again? 
# final_species <- read.csv("../data/species_trait_and_range_spp_level_only.csv")
# final_species <- read.csv("../data/species_trait_and_range_final_list.csv")


# # read in the mammal shape file
# mammals <- st_read("../data/IUCN_Maps/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
# species_only_binomial <- final_species[grep(" ", final_species$sci_name),]
# names_to_keep <- species_only_binomial$sci_name
# mammals <- mammals[(mammals$sci_name %in% names_to_keep),]
# cols_to_keep <- c("id_no", "sci_name", "SHAPE_Leng", "SHAPE_Area", "geometry")
# mammals <- mammals[cols_to_keep]
# mammals <- st_transform(mammals, 3857)
# area_values <- st_area(mammals$geometry)
# area_values <- units::set_units(area_values, "km^2")
# mammals$range_area_km <- area_values
# area_totals <- mammals %>% group_by(id_no) %>% summarize(total_area = sum(range_area_km))
# area_totals <- st_drop_geometry(area_totals)
# mammals <- mammals %>% left_join(area_totals, by = "id_no")
# mammals <- st_drop_geometry(mammals)
# keeps <- c("sci_name", "total_area")
# mammals <- mammals[keeps]
# mammals <- unique(mammals)
# final_species_updated <- full_join(final_species, mammals, by = "sci_name")
# final_species_updated$range_area_km <- final_species_updated$total_area
# final_species_updated$range_area_km <- as.numeric(final_species_updated$range_area_km)
# toDrop <- c('range_size', 'body_size', 'total_area')
# final_species_updated <- final_species_updated[, !colnames(final_species_updated) %in% toDrop]
# # also do trait data
# trait_data <- read.csv("../data/trait_data_reported.csv")
# toKeep <- c('iucn2020_binomial', 'adult_mass_g')
# trait_data <- trait_data[toKeep]
# names(trait_data)[names(trait_data) == 'iucn2020_binomial'] <- 'sci_name'
# trait_data$body_mass_kg <- trait_data$adult_mass_g / 1000
# trait_data <- trait_data[(trait_data$sci_name %in% names_to_keep),]
# trait_data <- trait_data[,!colnames(trait_data) %in% c('adult_mass_g')]
# # final_species_updated <- final_species_updated %>% full_join(trait_data, by = c('sci_name','body_mass_kg'))
# # drop the body_mass_kg col from final_species_updated and replace with new
# toDrop <- c('body_mass_kg')
# final_species_updated <- final_species_updated[, !colnames(final_species_updated) %in% toDrop]
# final_species_updated <- merge(final_species_updated, trait_data, by = 'sci_name', all=TRUE)
# bird_traits_and_ranges <- read.csv("../data/AVONET1_BirdLife.csv")
# toKeep <- c('Species1', 'Mass', 'Range.Size')
# bird_traits_and_ranges <- bird_traits_and_ranges[toKeep]
# bird_traits_and_ranges <- bird_traits_and_ranges %>% 
#   rename(
#     sci_name = Species1,
#     body_mass_kg = Mass,
#     range_area_km = Range.Size
#   )
# bird_traits_and_ranges$body_mass_kg <- bird_traits_and_ranges$body_mass_kg / 1000
# bird_traits_and_ranges <- bird_traits_and_ranges[(bird_traits_and_ranges$sci_name %in% names_to_keep),]
# # merge the IUCN data to the bird trait data, then drop from species df and remerge
# # make df of just mammals / things that are not in bird traits
# birds_to_drop <- bird_traits_and_ranges$sci_name
# birds_iucn <- final_species_updated[(final_species_updated$sci_name %in% birds_to_drop),]
# # drop empty cols
# birds_iucn <- birds_iucn[, colSums(!is.na(birds_iucn)) > 0]
# bird_traits_and_ranges <- merge(bird_traits_and_ranges, birds_iucn, by = 'sci_name')
# 
# # birds_to_drop <- as.list(birds_to_drop)
# final_species_updated <- final_species_updated[!final_species_updated$sci_name %in% birds_to_drop,]
# 
# # this one doesn't work properly
# final_species_updated <- merge(final_species_updated, bird_traits_and_ranges, all=TRUE) # this isn't working as intended
# # write out the final df for manual processing
# write.csv(final_species_updated, "../data/final_traits.csv", row.names = FALSE)
# 
# 
# # read it back in after some manual processing 
# new_species <- read.csv("../data/final_traits.csv")
# # pull out any rows with NA into a new frame
# to_do <- new_species %>% filter(if_any(everything(), is.na))
# # drop those rows from new_species 
# to_drop <- to_do$sci_name
# new_species <- new_species[(new_species$sci_name %in% to_drop),]
# 
# # now to process these ones again
# mammals <- st_read("../data/IUCN_Maps/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
# # drop geom for now
# mammals_no_geom <- st_drop_geometry(mammals)
# # write it out as a .csv 
# write.csv(mammals_no_geom, "../data/mammals_shapes.csv", row.names = FALSE)
# # write out the new species to manually update to correct sci names
# write.csv(new_species, "../data/species_to_correct.csv", row.names = FALSE)
# 
# read in the freshwater mammals shape file
freshwater <- st_read("../data/IUCN_Maps/MAMMALS_FRESHWATER/MAMMALS_FRESHWATER.shp")
# write it out without geoms
freshwater_no_geom <- st_drop_geometry(freshwater)
write.csv(freshwater_no_geom, "../data/freshwater.csv", row.names = FALSE)
# 
# # need to read in the now updated final_traits.csv, get traits and range for anything with NA in there
# final_updated <- read.csv("../data/final_traits_updated.csv")
# to_do <- final_updated %>% filter(if_any(everything(), is.na))
# to_drop <- to_do$sci_name
# final_updated <- final_updated[!(final_updated$sci_name %in% to_drop),]
# 
# # trait data sci names do not match up, if we already have bodysize do not query for it again
# 
# # now run through again.... should make a func for this
# # read in the mammal shape file
# mammals <- st_read("../data/IUCN_Maps/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
# species_only_binomial <- to_do[grep(" ", to_do$sci_name),]
# names_to_keep <- species_only_binomial$sci_name
# mammals <- mammals[(mammals$sci_name %in% names_to_keep),]
# cols_to_keep <- c("id_no", "sci_name", "SHAPE_Leng", "SHAPE_Area", "geometry")
# mammals <- mammals[cols_to_keep]
# mammals <- st_transform(mammals, 3857)
# area_values <- st_area(mammals$geometry)
# area_values <- units::set_units(area_values, "km^2")
# mammals$range_area_km <- area_values
# area_totals <- mammals %>% group_by(id_no) %>% summarize(total_area = sum(range_area_km))
# area_totals <- st_drop_geometry(area_totals)
# mammals <- mammals %>% left_join(area_totals, by = "id_no")
# mammals <- st_drop_geometry(mammals)
# keeps <- c("sci_name", "total_area")
# mammals <- mammals[keeps]
# mammals <- unique(mammals)
# to_do <- full_join(to_do, mammals, by = "sci_name")
# to_do$range_area_km <- to_do$total_area
# to_do$range_area_km <- as.numeric(to_do$range_area_km)
# toDrop <- c('range_size', 'body_size', 'total_area')
# to_do <- to_do[, !colnames(to_do) %in% toDrop]
# # merge back the rows that have no NA's? 
# still_to_do <- to_do %>% filter(if_any(everything(), is.na))
# to_drop <- still_to_do$sci_name
# to_do <- to_do[!(to_do$sci_name %in% to_drop),]
# # now merge them back to final_updated
# final_updated <- full_join(final_updated, to_do)
# 
# # also do trait data
# trait_data <- read.csv("../data/trait_data_reported.csv")
# toKeep <- c('iucn2020_binomial', 'adult_mass_g')
# trait_data <- trait_data[toKeep]
# names(trait_data)[names(trait_data) == 'iucn2020_binomial'] <- 'sci_name'
# trait_data$body_mass_kg <- trait_data$adult_mass_g / 1000
# trait_data <- trait_data[(trait_data$sci_name %in% names_to_keep),]
# trait_data <- trait_data[,!colnames(trait_data) %in% c('adult_mass_g')]
# # final_species_updated <- final_species_updated %>% full_join(trait_data, by = c('sci_name','body_mass_kg'))
# # drop the body_mass_kg col from final_species_updated and replace with new
# # toDrop <- c('body_mass_kg')
# # still_to_do <- still_to_do[, !colnames(still_to_do) %in% toDrop]
# still_to_do <- merge(still_to_do, trait_data, by = 'sci_name', all=TRUE)
# # Replace values conditionally
# still_to_do <- still_to_do %>%
#   mutate(body_mass_kg = coalesce(body_mass_kg.x, body_mass_kg.y)) %>%
#   select(-ends_with(".x"), -ends_with(".y"))
# # pull out and merge back complete rows again
# to_do <- still_to_do %>% filter(if_any(everything(), is.na))
# to_drop <- to_do$sci_name
# still_to_do <- still_to_do[!(still_to_do$sci_name %in% to_drop),]
# # now merge them back to final_updated
# final_updated <- full_join(final_updated, still_to_do)
to_do <- species_updated %>% filter(if_any(everything(), is.na))
names_to_keep <- species_updated$sci_name
# do freshwater here
# freshwater now
freshwater <- freshwater[(freshwater$sci_name %in% names_to_keep),]
cols_to_keep <- c("id_no", "sci_name", "SHAPE_Leng", "SHAPE_Area", "geometry")
freshwater <- freshwater[cols_to_keep]
freshwater <- st_transform(freshwater, 3857)
area_values <- st_area(freshwater$geometry)
area_values <- units::set_units(area_values, "km^2")
freshwater$range_area_km <- area_values
area_totals <- freshwater %>% group_by(id_no) %>% summarize(total_area = sum(range_area_km))
area_totals <- st_drop_geometry(area_totals)
freshwater <- freshwater %>% left_join(area_totals, by = "id_no")
freshwater <- st_drop_geometry(freshwater)
keeps <- c("sci_name", "total_area")
freshwater <- freshwater[keeps]
freshwater <- unique(freshwater)
to_do <- full_join(to_do, freshwater, by = "sci_name")
to_do$range_area_km <- to_do$total_area
to_do$range_area_km <- as.numeric(to_do$range_area_km)
toDrop <- c('range_size', 'body_size', 'total_area')
to_do <- to_do[, !colnames(to_do) %in% toDrop]
# merge back the rows that have no NA's? 
still_to_do <- to_do %>% filter(if_any(everything(), is.na))
to_drop <- still_to_do$sci_name
to_do <- to_do[!(to_do$sci_name %in% to_drop),]
# now merge them back to final_updated
# final_updated <- full_join(final_updated, to_do)
species_updated <- species_updated %>%
  left_join(to_do, by = "sci_name")
species_updated <- species_updated %>%
  mutate(
    body_mass_kg = coalesce(body_mass_kg.x, body_mass_kg.y),
    range_area_km = coalesce(range_area_km.x, range_area_km.y)
  ) %>%
  select(-body_mass_kg.x, -body_mass_kg.y, -range_area_km.x, -range_area_km.y)

# write it out and just do the manual update for the remaining spp
write.csv(species_updated, "../data/species_ranges_and_traits_final_final.csv", row.names = FALSE)

# bird_traits_and_ranges <- read.csv("../data/AVONET1_BirdLife.csv")
# toKeep <- c('Species1', 'Mass', 'Range.Size')
# bird_traits_and_ranges <- bird_traits_and_ranges[toKeep]
# bird_traits_and_ranges <- bird_traits_and_ranges %>% 
#   rename(
#     sci_name = Species1,
#     body_mass_kg = Mass,
#     range_area_km = Range.Size
#   )
# bird_traits_and_ranges$body_mass_kg <- bird_traits_and_ranges$body_mass_kg / 1000
# bird_traits_and_ranges <- bird_traits_and_ranges[(bird_traits_and_ranges$sci_name %in% names_to_keep),]
# # merge the IUCN data to the bird trait data, then drop from species df and remerge
# # make df of just mammals / things that are not in bird traits
# birds_to_drop <- bird_traits_and_ranges$sci_name
# birds_iucn <- still_to_do[(still_to_do$sci_name %in% birds_to_drop),]
# # drop empty cols
# birds_iucn <- birds_iucn[, colSums(!is.na(birds_iucn)) > 0]
# bird_traits_and_ranges <- merge(bird_traits_and_ranges, birds_iucn, by = 'sci_name')
# # birds_to_drop <- as.list(birds_to_drop)
# still_to_do <- still_to_do[!still_to_do$sci_name %in% birds_to_drop,]
# # this one doesn't work properly
# still_to_do <- merge(still_to_do, bird_traits_and_ranges, all=TRUE)
# # pull out and merge back complete rows again
# to_do <- still_to_do %>% filter(if_any(everything(), is.na))
# to_drop <- to_do$sci_name
# still_to_do <- still_to_do[!(still_to_do$sci_name %in% to_drop),]
# # now merge them back to final_updated
# final_updated <- full_join(final_updated, still_to_do)

# now reptiles for the iguana, and then manual I think
reptiles <- st_read("../data/IUCN_Maps/SCALED_REPTILES/SCALED_REPTILES_PART2.shp")
species_only_binomial <- to_do[grep(" ", to_do$sci_name),]
names_to_keep <- species_only_binomial$sci_name
reptiles <- reptiles[(reptiles$sci_name %in% names_to_keep),]
cols_to_keep <- c("id_no", "sci_name", "SHAPE_Leng", "SHAPE_Area", "geometry")
reptiles <- reptiles[cols_to_keep]
reptiles <- st_transform(reptiles, 3857)
area_values <- st_area(reptiles$geometry)
area_values <- units::set_units(area_values, "km^2")
reptiles$range_area_km <- area_values
area_totals <- reptiles %>% group_by(id_no) %>% summarize(total_area = sum(range_area_km))
area_totals <- st_drop_geometry(area_totals)
reptiles <- reptiles %>% left_join(area_totals, by = "id_no")
reptiles <- st_drop_geometry(reptiles)
keeps <- c("sci_name", "total_area")
reptiles <- reptiles[keeps]
reptiles <- unique(reptiles)

to_do <- full_join(to_do, reptiles, by = "sci_name")
to_do$range_area_km <- to_do$total_area
to_do$range_area_km <- as.numeric(to_do$range_area_km)
toDrop <- c('range_size', 'body_size', 'total_area')
to_do <- to_do[, !colnames(to_do) %in% toDrop]
# merge back the rows that have no NA's? 
# still_to_do <- to_do %>% filter(if_any(everything(), is.na))
# to_drop <- still_to_do$sci_name
# to_do <- to_do[!(to_do$sci_name %in% to_drop),]
# # now merge them back to final_updated
# final_updated <- full_join(final_updated, to_do)

# # write out both as csv's and manually merge / do it
# write.csv(final_updated, "../data/final_use_this.csv", row.names = FALSE)
# write.csv(to_do, "../data/to_add_to_final.csv", row.names = FALSE)
# 
# lontra_longicaudis <- read.csv("../data/to_do_lontra.csv")
# freshwater <- st_read("../data/IUCN_Maps/MAMMALS_FRESHWATER/MAMMALS_FRESHWATER.shp")
# species_only_binomial <- lontra_longicaudis[grep(" ", lontra_longicaudis$sci_name),]
# names_to_keep <- species_only_binomial$sci_name
# freshwater <- freshwater[(freshwater$sci_name %in% names_to_keep),]
# cols_to_keep <- c("id_no", "sci_name", "SHAPE_Leng", "SHAPE_Area", "geometry")
# freshwater <- freshwater[cols_to_keep]
# freshwater <- st_transform(freshwater, 3857)
# area_values <- st_area(freshwater$geometry)
# area_values <- units::set_units(area_values, "km^2")
# freshwater$range_area_km <- area_values
# area_totals <- freshwater %>% group_by(id_no) %>% summarize(total_area = sum(range_area_km))
# area_totals <- st_drop_geometry(area_totals)
# freshwater <- freshwater %>% left_join(area_totals, by = "id_no")
# freshwater <- st_drop_geometry(freshwater)
# keeps <- c("sci_name", "total_area")
# freshwater <- freshwater[keeps]
# freshwater <- unique(freshwater)
# 
# lontra_longicaudis <- full_join(lontra_longicaudis, freshwater, by = "sci_name")
# lontra_longicaudis$range_area_km <- lontra_longicaudis$total_area
# lontra_longicaudis$range_area_km <- as.numeric(lontra_longicaudis$range_area_km)
# toDrop <- c('range_size', 'body_size', 'total_area')
# lontra_longicaudis <- lontra_longicaudis[, !colnames(lontra_longicaudis) %in% toDrop]
# 
# write.csv(lontra_longicaudis, "../data/lontra_out.csv", row.names = FALSE)
# 
# # try for the pangolins
# once_more <- read.csv("../data/to_add_to_final.csv")
# # read in the mammal shape file
# mammals <- st_read("../data/IUCN_Maps/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
# species_only_binomial <- once_more[grep(" ", once_more$sci_name),]
# species_only_binomial
# names_to_keep <- species_only_binomial$sci_name
# mammals <- mammals[(mammals$sci_name %in% names_to_keep),]
# cols_to_keep <- c("id_no", "sci_name", "SHAPE_Leng", "SHAPE_Area", "geometry")
# mammals <- mammals[cols_to_keep]
# mammals <- st_transform(mammals, 3857)
# area_values <- st_area(mammals$geometry)
# area_values <- units::set_units(area_values, "km^2")
# mammals$range_area_km <- area_values
# area_totals <- mammals %>% group_by(id_no) %>% summarize(total_area = sum(range_area_km))
# area_totals <- st_drop_geometry(area_totals)
# mammals <- mammals %>% left_join(area_totals, by = "id_no")
# mammals <- st_drop_geometry(mammals)
# keeps <- c("sci_name", "total_area")
# mammals <- mammals[keeps]
# mammals <- unique(mammals)
# once_more_updated <- full_join(once_more, mammals, by = "sci_name")
# once_more_updated$range_area_km <- once_more_updated$total_area
# once_more_updated$range_area_km <- as.numeric(once_more_updated$range_area_km)
# toDrop <- c('range_size', 'body_size', 'total_area')
# once_more_updated <- once_more_updated[, !colnames(once_more_updated) %in% toDrop]
# # also do trait data
# trait_data <- read.csv("../data/trait_data_reported.csv")
# toKeep <- c('iucn2020_binomial', 'adult_mass_g')
# trait_data <- trait_data[toKeep]
# names(trait_data)[names(trait_data) == 'iucn2020_binomial'] <- 'sci_name'
# trait_data$body_mass_kg <- trait_data$adult_mass_g / 1000
# trait_data <- trait_data[(trait_data$sci_name %in% names_to_keep),]
# trait_data <- trait_data[,!colnames(trait_data) %in% c('adult_mass_g')]

