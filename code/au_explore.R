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

# Remove the AU part of the label
aufreq$shortname <- aufreq %>% 
  pull(name) %>% 
  gsub("[[:space:]]+AU$", "", x = .) %>% 
  gsub("Alternate Universe - ", replacement = "", x = .)

# Remove (RWBY) labels
aufreq %>% filter(str_detect(shortname, "RWBY"))  # check first
aufreq$shortname <- aufreq %>% 
  pull(shortname) %>% 
  gsub(" (RWBY)", "", x = ., fixed = TRUE)
  

# Remove particular rows that I don't want
aufreq <- aufreq %>% 
  filter(!str_detect(shortname, "SLEEP")) %>% 
  filter(!str_detect(shortname, "Alternate Universe")) # the generic tag


# Works! Needed up update Rcpp, apparently
wordcloud(aufreq$shortname, aufreq$n, scale = c(4, 0.5), min.freq = 5, 
          rot.per = 0.2, colors = brewer.pal(8, "Dark2"))

set.seed(18)
wordcloud(aufreq$shortname, aufreq$n, scale = c(4, 0.6), min.freq = 10, 
          rot.per = 0, random.order = FALSE, random.color = TRUE, 
          colors = brewer.pal(8, "Dark2"), fixed.asp = FALSE)
