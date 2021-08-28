# Read in tagged works info for a fandom and pull info on AU tags.


library(tidyverse)
library(wordcloud)


# Import data
(load("data/works_RWBY.Rda"))
(load("data/tags_RWBY.Rda"))

wtagged <- wtagged_fandom
rm(wtagged_fandom)


## Find AU tags
freetags <- tred %>% filter(type == "Freeform")

temppat <- c("Alternate [Uu]niverse", "AU")

autags <- freetags %>% 
  filter(str_detect(name, pattern = temppat[1]) | 
         str_detect(name, pattern = temppat[2]))

# Trying to do better job of eliminating false positives
#tempau <- freetags %>% 
#  filter(str_detect(name, pattern = temppat[1]) | 
#           str_detect(name, pattern = "AU\b"))

# Filter works frame to rows with one of those tags
auworks <- wtagged %>% 
  filter(tag_list %in% autags$id) %>% 
  select(wid) %>% 
  unique

wkau <- wtagged %>% filter(wid %in% pull(auworks))


## Plot wordcloud of AU tags

# Store frequency of tags
aufreq <- wtagged %>% 
  filter(tag_list %in% autags$id) %>% 
  count(name, sort = TRUE) 

# Remove particular rows that I don't want
aufreq <- aufreq %>% 
  filter(!str_detect(name, "SLEEP"))

# Remove the AU part of the label
aufreq$shortname <- aufreq %>% 
  pull(name) %>% 
  gsub("[[:space:]]+AU$", "", x = .) %>% 
  gsub("Alternate Universe - ", replacement = "", x = .)

# This hangs, need to fix it
wordcloud(aufreq$shortname, aured$n, min.freq = 2)

