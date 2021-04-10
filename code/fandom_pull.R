# Pull all rows for a specific fandom tag from the works frame


library(tidyverse)
library(ggplot2)


# Import data
(load("data/work_samp.Rda"))
(load("data/tag_samp.Rda"))

tdf <- tag_samp
wdf <- work_samp

# See what fandom(s) are available to try
fandoms <- tdf %>% filter(type == "Fandom") 
ftags <- fandoms %>% select(id, name, merger_id)


## Pick one (hello, Supernatural) and filter works for those entries

# catches any occurrence of that string, need to force exact match
#pat <- paste0("\\+?", ftags$id[1], "\\+?")  

# Next try: non-digit, integer code of interest, non-digit
pat <- paste0("[^[:digit:]]", ftags$id[1], "[^[:digit:]]")
wdf %>% 
  filter(grepl(pattern = pat, x = tags)) %>% 
  select(tags)
# This matches +tag+, does it catch at beginning or end of line too?

# Try with 9 (shows up at beginning) and 16 (shows up at end)
pat2 <- paste0("[^[:digit:]]", 9, "[^[:digit:]]")   # doesn't work
pat3 <- paste0("[^[:digit:]]", 16, "[^[:digit:]]")

wdf %>% 
  filter(grepl(pattern = pat2, x = tags)) #%>% 
  #select(tags)

# What needs to match: 
# 1. Start of line, code, then +
# 2. +, code, +
# 3. + (or not!), code, end of line

# Phrased differently: The code with no digits in front of it or after it
#pat4 <- paste0("[^[:digit:]]*[+]?", ftags$id[1], "[^[:digit:]]*")
pat4 <- paste0("(^|[^[:digit:]])", ftags$id[1], "([^[:digit:]]|$)")
pat4a <- paste0("(^|[^[:digit:]])", 9, "([^[:digit:]]|$)")
pat4b <- paste0("(^|[^[:digit:]])", 16, "([^[:digit:]]|$)")

wdf %>% 
  filter(grepl(pattern = pat4, x = tags)) 

wdf %>% 
  filter(grepl(pattern = pat4a, x = tags)) 

wdf %>% 
  filter(grepl(pattern = pat4b, x = tags)) 

# Ah-hah! I think this works.