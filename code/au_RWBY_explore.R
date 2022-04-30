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

tagpat <- c("Alternate [Uu]niverse", "AU")

autags <- freetags %>% 
  filter(str_detect(name, pattern = tagpat[1]) | 
         str_detect(name, pattern = tagpat[2]))

# Trying to do better job of eliminating false positives
#tempau <- freetags %>% 
#  filter(str_detect(name, pattern = temppat[1]) | 
#           str_detect(name, pattern = "AU\b"))

# Filter works frame to rows with one of those tags
# auworks <- wtagged %>% 
#   filter(tag_list %in% autags$id) %>% 
#   select(wid) %>% 
#   unique
# 
# wkau <- wtagged %>% filter(wid %in% pull(auworks))


## Pull works with particular tag(s)

gettaggedwks <- function(works, tag, tag_col = "name") {
  wids <- NULL
  for (i in seq_along(tag)) {  # loop through specified tag patterns
    cat(tag[i], "\n")
    tempwid <- works %>% 
      filter(str_detect(.data[[tag_col]], pattern = tag[i])) %>% 
      select(wid)
    wids <- bind_rows(wids, tempwid)
  }
  df <- works %>% filter(wid %in% pull(wids))
  
  return(df)
}

gettaggedwks(wtagged, tag = tagpat)


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
  #filter(!str_detect(shortname, "SLEEP")) %>% 
  filter(!str_detect(tolower(shortname), "author")) %>% 
  filter(!str_detect(shortname, "Alternate Universe")) # the generic tag


# Plot a wordcloud
wordcloud(aufreq$shortname, aufreq$n, scale = c(4, 0.5), min.freq = 5, 
          rot.per = 0.2, colors = brewer.pal(8, "Dark2"))

set.seed(18)
wordcloud(aufreq$shortname, aufreq$n, scale = c(4, 0.6), min.freq = 10, 
          rot.per = 0, random.order = FALSE, random.color = TRUE, 
          colors = brewer.pal(8, "Dark2"), fixed.asp = FALSE)

set.seed(10)
wordcloud(aufreq$shortname, aufreq$n, scale = c(4, 0.6), min.freq = 15, 
          rot.per = 0, random.order = FALSE, #random.color = TRUE, 
          colors = brewer.pal(8, "Dark2"), fixed.asp = FALSE)


## Collect info about works with AU tags

# Number of AU tags on the work
aucount <- wkau %>% 
  filter(tag_list %in% autags$id) %>% 
  group_by(wid) %>% 
  count(wid, name = "auCt") %>% 
  left_join(wkau) %>% 
  filter(tag_list %in% autags$id) %>% 
  select(-restricted, -type) %>% 
  relocate(auCt, .after = last_col()) %>% 
  ungroup()


## Find first non-crossover work in fandom

# Count fandoms per work
fandomcount <- wtagged %>% 
  filter(type == "Fandom") %>% 
  group_by(wid) %>% 
  count(wid, name = "fandomCt") %>% 
  left_join(wtagged) %>% 
  filter(type == "Fandom") %>%  # save fandom names for now
  ungroup()

fandomcount %>% distinct(name, .keep_all = TRUE)  # pick minimum date row?
firstwk <- fandomcount %>% 
  filter(fandomCt == 1) %>% 
  distinct(name, .keep_all = TRUE)  # earliest row excluding crossovers

firstwk$creat_date

# Save age (in that fandom) of each AU-tagged work
aucount$fandomAge <- aucount$creat_date - firstwk$creat_date