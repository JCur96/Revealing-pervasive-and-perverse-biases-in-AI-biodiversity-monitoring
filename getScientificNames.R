library(taxize)
setwd("code")
# read in the image level data
df.image_level <- read.csv("../data/image_level_data.csv")
# get unique common names
df.unique <- unique(df.image_level['correct_label'])
df.unique$sci_name <- ''
# ENTREZ_KEY='KEY_HERE'
# use_entrez()
# usethis::edit_r_environ()
for (i in 1:nrow(df.unique)) {
  # print(i)
  df.unique$sci_name[i] <- comm2sci(df.unique$correct_label[i])
}

# write out the df.unique, so as to process those names that no matches were found for
df.unique <- as.data.frame(df.unique)
# remove all values that are character(0)
for (i in 1:nrow(df.unique)) {
  if (df.unique$sci_name[i] == "character(0)") {
    df.unique$sci_name[i] = ''
  } 
  
}
df.unique <- apply(df.unique,2,as.character)
write.csv(df.unique, "../data/species_names.csv")