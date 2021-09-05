# Read in full works list and pull info on AU tags.


library(tidyverse)
library(wordcloud)


# Import data
#(load("data/works_merged.Rda"))
#(load("data/tags.Rda"))
(load("data/works_tagged.Rda"))  # One row per tag, duplicate tags merged
(load("data/tags_merged.Rda"))

works <- wtagged
rm(wtagged)

tags <- tags_merged
rm(tags_merged)

## Find AU tags
freetags <- tags %>% filter(type == "Freeform")
#freetags2 <- tags_merged %>% filter(type == "Freeform")

tagpat <- c("Alternate [Uu]niverse", "AU")

autags <- freetags %>% 
  filter(str_detect(name, pattern = tagpat[1]) | 
           str_detect(name, pattern = tagpat[2]))

# Look for false positives
autags %>% filter(str_detect(name, pattern = "\\wAU|AU\\w")) %>% View()

# Filter works frame to rows with one of those tags
auworks <- works %>% 
  filter(tag %in% autags$id) %>% 
  select(wid) %>% 
  unique()

wkau <- works %>% filter(wid %in% pull(auworks))


# Save frame of info for these works

# Get list of fandoms
fandoms <- tags %>% 
  filter(type == "Fandom") %>% 
  select(-type)

# Save first instance of each fandom tag in works list

# Get memory errors if we run this straight. Solution courtesy of: 
#  https://www.researchgate.net/post/How_to_solve_Error_cannot_allocate_vector_of_size_12_Gb_in_R
if(.Platform$OS.type == "windows") withAutoprint({
  memory.size()
  memory.size(TRUE)
  memory.limit()
})
memory.limit(size=56000)


#works %>% filter(tag %in% fandoms$id) %>% distinct(tag, .keep_all = TRUE)
firstwk <- works %>% 
  filter(tag %in% fandoms$id) %>% 
  distinct(tag, .keep_all = TRUE) %>% 
  select(-restricted, -complete, -word_count)
# This is 56K whereas there are 75K fandom tags, which ones are missing?

# Check on missing fandoms
fandoms %>% 
  filter(!(id %in% firstwk$tag)) %>% 
  filter(cached_count > 0) %>% 
  arrange(desc(cached_count))
# Spot-checking the first few, filtering works to those tags finds none; these
#  are fandom tags with no remaining uses?

# Remove big works frame to spare memory
rm(works)


## Plot wordcloud of AU tags

# Store frequency of tags
# **Why is this a different size than the length of autags?
aufreq <- wkau %>% 
  filter(tag %in% autags$id) %>% 
  count(tag_name, sort = TRUE) 

# Remove the AU part of the label
aufreq$shortname <- aufreq %>% 
  pull(tag_name) %>% 
  gsub("[[:space:]]+AU$", "", x = .) %>% 
  gsub("Alternate Universe *[-:?] *", replacement = "", x = .) %>% 
  gsub("Alternate Universe", replacement = "AU", x = .)

# Remove particular rows that I don't want
# Okay, this is messier than the single-fandom version, need to clean up
aufreq %>% filter(str_detect(shortname, "Alternate Universe"))
aufreq %>% filter(str_detect(shortname, "AU"))

# Remove the generic AU tag
aufreq <- aufreq %>% 
  filter(!(shortname == "AU")) # the generic tag

# Frequency of AU tags
aufreq %>% 
  ggplot(mapping = aes(x = n)) + 
  geom_histogram() + 
  scale_x_log10() + 
  scale_y_log10()


# Works! Needed up update Rcpp, apparently
wordcloud(aufreq$shortname, aufreq$n, scale = c(4, 0.5), min.freq = 1000, 
          rot.per = 0.2, colors = brewer.pal(8, "Dark2"))

set.seed(1)
wordcloud(aufreq$shortname, aufreq$n, scale = c(4, 0.6), min.freq = 3000, 
          rot.per = 0, random.order = FALSE, random.color = TRUE, 
          colors = brewer.pal(8, "Dark2"), fixed.asp = FALSE)
