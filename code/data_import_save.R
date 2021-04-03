# Import the works and tags .csv files, do a bit of preliminary cleanup, and 
# save as .Rda files for further processing.

library(tidyverse)

# Import data
tags <- read_csv("data/tags-20210226.csv")
works <- read_csv("data/works-20210226.csv")


# Spaces in column names are annoying, ditch that
names(works)[1] <- "creat_date"

# Remove column of NAs (made by extra comma at end of header row)
works <- works %>% select(-X7)


# Save them to .Rda files (individually because huge)
save(tags, file = "tags.Rda")
save(works, file = "works.Rda")
