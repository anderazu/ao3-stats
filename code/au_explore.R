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

# Filter works frame to rows with one of those tags
auworks <- wtagged %>% 
  filter(tag_list %in% autags$id) %>% 
  select(wid) %>% 
  unique

wkau <- wtagged %>% filter(wid %in% pull(auworks))


